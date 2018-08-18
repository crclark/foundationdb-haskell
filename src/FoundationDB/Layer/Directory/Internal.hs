{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FoundationDB.Layer.Directory.Internal where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Data.Monoid
import Data.List (intercalate)
import Data.Serialize.Get (runGet, getWord32le)
import Data.Serialize.Put (runPut, putWord32le)
import Data.Text (Text)
import Data.Word (Word32)

import FoundationDB
import FoundationDB.Layer.Directory.Internal.Error
import FoundationDB.Layer.Directory.Internal.HCA
import FoundationDB.Layer.Directory.Internal.Node
import FoundationDB.Layer.Subspace
import qualified FoundationDB.Layer.Tuple as Tuple

_SUBDIRS :: Int
_SUBDIRS = 0

majorVersion, minorVersion, microVersion :: Word32
majorVersion = 1
minorVersion = 0
microVersion = 0

throwing :: String -> Either a b -> Transaction b
throwing s = either (const $ throwDirError s) return

--TODO: stuff can break if users change parts of the internals of this thing.
-- Don't export.
data DirectoryLayer = DirectoryLayer
  { nodeSS :: Subspace
    -- ^ Subspace for directory metadata.
  , contentSS :: Subspace
    -- ^ Subspace for directory content.
  , allocator :: HCA
  , rootNode :: Subspace
  -- TODO: allowManualPrefixes is only used in createOrOpen. Eliminate?
  , allowManualPrefixes :: Bool
  , dlPath :: [Text]
  } deriving (Show, Eq, Ord)

data DirSubspace = DirSubspace
  { dirSubspace :: Subspace
  , dirSubspaceDL :: DirectoryLayer
  , dirSubspacePath :: [Text]
  , dirSubspaceLayer :: ByteString
  } deriving (Show, Eq, Ord)

data DirPartition = DirPartition
  { dirPartition :: DirectoryLayer
  , dirPartitionParentDL :: DirectoryLayer
  } deriving (Show, Eq, Ord)

newDirectoryLayer
  :: Subspace
                  -- ^ node subspace for directory metadata
  -> Subspace
                  -- ^ content subspace
  -> Bool
                  -- ^ allow manual prefixes
  -> DirectoryLayer
newDirectoryLayer nodeSS contentSS allowManualPrefixes =
  let rootNode  = extend nodeSS [Tuple.BytesElem (subspaceKey nodeSS)]
      allocator = newHCA (extend rootNode [Tuple.BytesElem "hca"])
      dlPath    = []
  in  DirectoryLayer {..}

defaultDirLayer :: DirectoryLayer
defaultDirLayer = newDirectoryLayer (Subspace "\xfe") (Subspace "") False

-- TODO: why not combine this with newDirectoryLayer?
-- then we don't have the clumsiness of always passing a prefix and having
-- to check the allowManualPrefixes bool. Could just pass Maybe Prefix instead.
-- Or is this creating or opening a subdirectory of the input DL?
-- | Open a directory. Returns 'Nothing' if the directory doesn't exist.
open
  :: DirectoryLayer
  -> [Text]
     -- ^ path
  -> ByteString
     -- ^ layer
  -> (Maybe ByteString)
     -- ^ optional custom prefix
  -> Transaction (Maybe DirSubspace)
open _  []   _     _       = throwDirError "Can't open root directory"
-- TODO: prefix won't be used until we add partition support
open dl path layer _prefix = do
  checkVersion dl
  existingNode <- find dl path
  if nodeExists existingNode
    then do
      existingLayer <- getNodeLayer existingNode
      when (layer /= existingLayer)
           (throwDirError "directory created with incompatible layer")
      Just
        <$> contentsOfNodeSubspace dl
                                   (fromJust $ nodeNodeSS existingNode)
                                   (nodePath existingNode)
                                   existingLayer
    else return Nothing

-- TODO: this can fail in various ways. Some of the failure modes can be user
-- bugs -- might be better to return a sum type.
createOrOpen
  :: DirectoryLayer
  -> [Text]
          -- ^ path
  -> ByteString
          -- ^ layer
  -> (Maybe ByteString)
          -- ^ optional custom prefix
  -> Transaction DirSubspace
createOrOpen _ [] _ _ = throwDirError "Can't open root directory"
createOrOpen dl@DirectoryLayer {..} path layer prefix = do
  tryOpen <- open dl path layer prefix
  case tryOpen of
    Just ss -> return ss
    Nothing -> do
      prefixToUse <- case prefix of
        Nothing -> do
          newDirPrefix  <- allocate allocator contentSS
          isPrefixEmpty <- isRangeEmpty (subspaceRange newDirPrefix)
          when (not isPrefixEmpty)
               (throwDirError "Failed to alloc new dir: prefix not empty.")
          isFree <- isPrefixFree dl (pack newDirPrefix [])
          when
            (not isFree)
            (throwDirError
            $ "A manually allocated prefix conflicts with the one chosen by the allocator: "
            ++ (show newDirPrefix)
            )
          return (pack newDirPrefix [])
        Just prefixBytes -> do
          isFree <- isPrefixFree dl prefixBytes
          when (not isFree)
               (throwDirError $ "The requested prefix is already in use.")
          return prefixBytes
      parentNode <- if length path > 1
        then do
          pd <- createOrOpen dl (init path) "" Nothing
          let pdk = subspaceKey (dirSubspace pd)
          return $ nodeWithPrefix dl pdk
        else return rootNode
      --TODO: null check of parentNode here in Go code
      let node = nodeWithPrefix dl prefixToUse
      set
        (pack parentNode [Tuple.IntElem _SUBDIRS, Tuple.TextElem (last path)])
        prefixToUse
      set (pack node [Tuple.BytesElem "layer"]) layer
      contentsOfNodeSubspace dl node path layer

exists :: DirectoryLayer -> [Text] -> Transaction Bool
exists dl path = nodeExists <$> find dl path

-- | List the contents of a directory. Returns 'Nothing' if the directory
-- does not exist.
list :: DirectoryLayer -> [Text] -> Transaction (Maybe [Text])
list dl path = do
  node <- find dl path
  if nodeExists node
     -- TODO: fix up this node type to remove fromJust
    then Just <$> subdirNames dl (fromJust $ nodeNodeSS node)
    else return Nothing

data MoveError =
  SelfSubDir
  -- ^ returned by 'move' if you attempt to move a directory into a subdirectory
  --   of itself.
  | SourceDoesNotExist
  -- ^ Returned by 'move' if the source subdirectory does not exist.
  | MoveBetweenPartitions
  -- ^ Returned by 'move' if you attempt to move a directory from one partition
  --   to another.
  | DestinationAlreadyExists
  -- ^ Returned by 'move' if the destination directory already exists.
  | DestinationParentDoesNotExist
  -- ^ Returned by 'move' if the parent of the destination directory doesn't
  --   already exist.
  | CannotMoveToRoot
  -- ^ Returned by 'move' if the destination path is the root path.
  deriving (Show, Eq, Ord)

move
  :: DirectoryLayer
  -> [Text]
     -- ^ from path
  -> [Text]
     -- ^ to path
  -> Transaction (Maybe MoveError)
move _  _       []      = return $ Just CannotMoveToRoot
move dl oldPath newPath = do
  let sliceEnd = min (length oldPath) (length newPath)
  if oldPath == take sliceEnd newPath
    then return (Just SelfSubDir)
    else do
      oldNode    <- find dl oldPath
      newNode    <- find dl newPath
      parentNode <- find dl (init newPath)
      case (nodeExists oldNode, nodeExists newNode, nodeExists parentNode) of
        (False, _   , _    ) -> return (Just SourceDoesNotExist)
        (_    , True, _    ) -> return (Just DestinationAlreadyExists)
        (_    , _   , False) -> return (Just DestinationParentDoesNotExist)
        _                    -> do
          let k = pack
                (fromJust $ nodeNodeSS parentNode)
                [Tuple.IntElem _SUBDIRS, Tuple.TextElem (last newPath)]
          let ve =
                unpack (nodeSS dl) (rawPrefix $ fromJust $ nodeNodeSS oldNode)
          case ve of
            Left e -> throwDirError $ "move failure: " ++ e
            Right (Tuple.BytesElem v : _) -> do
              set              k  v
              removeFromParent dl oldPath
              return Nothing
            _ -> throwDirError "move unpack failure"

-- | Remove a directory path, its contents, and all subdirectories.
-- Returns 'True' if removal succeeds. Fails for nonexistent paths and the root
-- directory.
-- TODO: just return ()?
remove
  :: DirectoryLayer
  -> [Text]
       -- ^ path
  -> Transaction Bool
-- can't remove root dir
remove _  []   = return False
remove dl path = do
  node <- find dl path
  if nodeExists node
    then do
      removeRecursive  dl (fromJust (nodeNodeSS node))
      removeFromParent dl path
      return True
    else return False

removeRecursive
  :: DirectoryLayer
  -> Subspace
                -- ^ node
  -> Transaction ()
removeRecursive dl@DirectoryLayer {..} node = do
  nodes <- subdirNodes dl node
  forM_ nodes (removeRecursive dl)
  let p = unpack nodeSS (pack node [])
  case p of
    Left  e                        -> throwDirError $ "removeRecursive: " ++ e
    Right (Tuple.BytesElem p' : _) -> case rangeKeys <$> prefixRange p' of
      Just (start, end) -> clearRange start end
      Nothing ->
        throwDirError
          $  "removeRecursive: couldn't make prefix range:"
          ++ show p'
    Right _ -> throwDirError "node unpacked to non-bytes tuple element"
  uncurry clearRange (rangeKeys (subspaceRange node))

removeFromParent :: DirectoryLayer -> [Text] -> Transaction ()
removeFromParent _  []   = return ()
removeFromParent dl path = do
  parent <- find dl (init path)
  case parent of
    (Node Nothing _ _) -> throwDirError $ "parent not found for " ++ show path
    (Node (Just sub) _ _) ->
      clear (pack sub [Tuple.IntElem _SUBDIRS, Tuple.TextElem (last path)])

-- TODO: should use Node type, except it's bizarre and broken.
-- existing Node type should probably be renamed NodeQueryResult or
-- something, and a real Node type introduced.
subdirNameNodes
  :: DirectoryLayer
  -> Subspace
                -- ^ node
  -> Transaction [(Text, Subspace)]
subdirNameNodes dl@DirectoryLayer {..} node = do
  let sd = extend node [Tuple.IntElem _SUBDIRS]
  rr  <- getRange (subspaceRange sd) >>= await
  kvs <- go rr
  let unpackKV (k, v) = case unpack sd k of
        Right [Tuple.TextElem t] -> return (t, nodeWithPrefix dl v)
        _ -> throwDirError "failed to unpack node name in subdirNameNodes"
  mapM unpackKV kvs

  -- TODO: don't use list or mapM above
 where
  go (RangeDone xs     ) = return xs
  go (RangeMore xs more) = do
    rr  <- await more
    xs' <- go rr
    return (xs ++ xs')

subdirNames :: DirectoryLayer -> Subspace -> Transaction [Text]
subdirNames dl node = fmap (map fst) (subdirNameNodes dl node)

subdirNodes :: DirectoryLayer -> Subspace -> Transaction [Subspace]
subdirNodes dl node = fmap (map snd) (subdirNameNodes dl node)

nodeContainingKey
  :: DirectoryLayer -> ByteString -> Transaction (Maybe Subspace)
nodeContainingKey dl@DirectoryLayer {..} k
  | BS.isPrefixOf (pack nodeSS []) k = return $ Just rootNode
  | otherwise = do
    let r = Range
          (rangeBegin $ subspaceRange nodeSS)
          (FirstGreaterOrEq (pack nodeSS [Tuple.BytesElem k] <> "\x00"))
          (Just 1)
          True
    rr <- getEntireRange r
    case rr of
      -- TODO: consider dropping [] from RangeDone constructor. Looks like
      -- underlying function is returning RangeMore [x] when there is only one
      -- item in the range.
      []       -> return Nothing
      (kv : _) -> processKV kv
 where
  processKV (k', _) = do
    let unpacked = unpack nodeSS k'
    case unpacked of
      Left  _ -> throwDirError "Failed to unpack in nodeContainingKey"
      Right (Tuple.BytesElem prevPrefix : _) -> if BS.isPrefixOf prevPrefix k
        then return (Just $ nodeWithPrefix dl prevPrefix)
        else return Nothing
      --TODO: there are two cases where we fail this way. Is this correct?
      -- Is this case actually possible?
      Right _ -> throwDirError "node unpacked to non-bytes element"


isPrefixFree :: DirectoryLayer -> ByteString -> Transaction Bool
isPrefixFree dl@DirectoryLayer {..} prefix
  | BS.length prefix == 0 = return False
  | otherwise = do
    nck <- nodeContainingKey dl prefix
    case nck of
      Just _  -> return False
      Nothing -> case prefixRange prefix of
        Nothing -> return False
        Just r  -> do
          let (bk, ek) = rangeKeys r
          -- TODO: other bindings have a KeyRange type for this common case
          -- we should probably have a keyRange function.
          let r' = Range
                (FirstGreaterOrEq (pack nodeSS [Tuple.BytesElem bk]))
                (FirstGreaterOrEq (pack nodeSS [Tuple.BytesElem ek]))
                Nothing
                False
          isRangeEmpty r'


checkVersion :: DirectoryLayer -> Transaction ()
checkVersion dl@DirectoryLayer {..} = do
  mver <- get (pack rootNode [Tuple.BytesElem "version"]) >>= await
  case mver of
    Nothing       -> initializeDirectory dl
    Just verBytes -> do
      (major, minor, micro) <-
        throwing "Couldn't parse directory version!" $ runGet
          ((,,) <$> getWord32le <*> getWord32le <*> getWord32le)
          verBytes
      when (major > majorVersion) $ throwDirError $ dirVersionError major
                                                                    minor
                                                                    micro
      when (minor > minorVersion)
        $  throwDirError
        $  dirVersionError major minor micro
        ++ ". Read-only access not supported."
 where
  dirVersionError major minor micro =
    "Can't open directory of version "
      ++ intercalate "." (map show [major, minor, micro])
      ++ " using directory layer code version "
      ++ intercalate "." (map show [majorVersion, minorVersion, microVersion])

initializeDirectory :: DirectoryLayer -> Transaction ()
initializeDirectory DirectoryLayer {..} = do
  let verBytes = runPut $ do
        putWord32le majorVersion
        putWord32le minorVersion
        putWord32le microVersion
  set (pack rootNode [Tuple.BytesElem "version"]) verBytes

nodeWithPrefix :: DirectoryLayer -> ByteString -> Subspace
nodeWithPrefix DirectoryLayer {..} prefix =
  extend nodeSS [Tuple.BytesElem prefix]

-- | Returns the longest prefix of @path@ that doesn't exist. If the entire
-- path exists, returns it.
find
  :: DirectoryLayer
  -> [Text]
     -- ^ path
  -> Transaction Node
find dl@DirectoryLayer {..} queryPath = go baseNode (zip [0 ..] queryPath)
 where

  baseNode = Node (Just rootNode) [] queryPath

  go n []            = return n
  go n ((i, p) : ps) = do
    let nodePrefixKey = pack (fromJust $ nodeNodeSS n)
                             [Tuple.IntElem _SUBDIRS, Tuple.TextElem p]
    nodePrefix <- get nodePrefixKey >>= await
    let n' = Node (fmap (nodeWithPrefix dl) nodePrefix)
                  (take (i + 1) queryPath)
                  []
    if not (nodeExists n')
      then return n'
      else do
        layer <- getNodeLayer n'
        if layer == "partition" then return n' else go n' ps

contentsOfNodePartition
  :: DirectoryLayer
  -> Subspace
  -- ^ node
  -> [Text]
  -- ^ path
  -> Transaction DirPartition
contentsOfNodePartition dl@DirectoryLayer {..} node queryPath = do
  -- TODO: need a type class of things that can be converted to keys to avoid
  -- @pack node []@
  -- TODO: do combinators like throwing already exist in some lib?
  p <- throwing "can't unpack node!" (unpack nodeSS (pack node []))
  --TODO: not total
  let (Tuple.BytesElem prefix) = head p
  let newPath                  = dlPath <> queryPath
  let newDL = newDirectoryLayer
        (subspace [Tuple.BytesElem (prefix <> "\xfe")])
        contentSS
        False
  return (DirPartition newDL { dlPath = newPath } dl)

contentsOfNodeSubspace
  :: DirectoryLayer
  -> Subspace
  -> [Text]
  -> ByteString
  -> Transaction DirSubspace
contentsOfNodeSubspace dl@DirectoryLayer {..} node queryPath layer = do
  -- TODO: need a type class of things that can be converted to keys to avoid
  -- @pack node []@
  p <- throwing "can't unpack node!" (unpack nodeSS (pack node []))
  case p of
    [Tuple.BytesElem prefixBytes] -> do
      let newPath = dlPath <> queryPath
      return $ DirSubspace (Subspace prefixBytes) dl newPath layer
    _ -> throwDirError "unexpected contents for node prefix value"
