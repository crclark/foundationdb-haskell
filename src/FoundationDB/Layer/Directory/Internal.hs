{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FoundationDB.Layer.Directory.Internal where

import Control.Monad (when, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (forM_)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Sequence(Seq(Empty,(:<|)))
import Data.Serialize.Get (runGet, getWord32le)
import Data.Serialize.Put (runPut, putWord32le)
import Data.Text (Text)
import Data.Traversable (mapM)
import Data.Word (Word32)

import FoundationDB
import FoundationDB.Error
import FoundationDB.Layer.Directory.Internal.Error
import FoundationDB.Layer.Directory.Internal.HCA
import FoundationDB.Layer.Directory.Internal.Node
import FoundationDB.Layer.Subspace
import qualified FoundationDB.Layer.Tuple as Tuple

_SUBDIRS :: Integer
_SUBDIRS = 0

majorVersion, minorVersion, microVersion :: Word32
majorVersion = 1
minorVersion = 0
microVersion = 0

throwing :: String -> Either a b -> Transaction b
throwing s = either (const $ throwDirInternalError s) return

-- | Represents a directory tree. A value of this type must be supplied to all
-- functions in this module.
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

-- | A path is a list of unicode strings.
type Path = [Text]

-- | Represents a single directory.
data Directory = Directory
  { directorySubspace :: Subspace
  , directoryPath :: Path
  , directoryLayer :: ByteString
  } deriving (Show, Eq, Ord)

-- | Gets the content subspace of a directory, which can be used to store
-- tuple-based keys.
dirSubspace :: Directory -> Subspace
dirSubspace = directorySubspace

-- | Gets the path of a directory.
dirPath :: Directory -> Path
dirPath = directoryPath

-- | Gets the layer tag that was specified when the directory was created.
dirLayer :: Directory -> ByteString
dirLayer = directoryLayer

-- TODO: this library doesn't yet support partitions. Some code exists, but it's
-- not yet used.
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
  let rootNode  = extend nodeSS [Tuple.Bytes (subspaceKey nodeSS)]
      allocator = newHCA (extend rootNode [Tuple.Bytes "hca"])
      dlPath    = []
  in  DirectoryLayer {..}

-- | The default directory layer has node subspace prefix @0xfe@.
-- This corresponds to using the defaults for all arguments to the
-- @DirectoryLayer@ constructor in other languages' bindings.
defaultDirLayer :: DirectoryLayer
defaultDirLayer = newDirectoryLayer (Subspace "\xfe") (Subspace "") False

-- | Tries to open a directory at the given path.
--   If the directory exists, returns it. Otherwise, returns 'Nothing'.
open :: DirectoryLayer
     -> Path
     -> Transaction (Maybe Directory)
open dl p = open' dl p "" Nothing

-- | Opens a directory at the given path. If the directory does not exist, it
-- is created.
createOrOpen :: DirectoryLayer
             -> Path
             -> Transaction Directory
createOrOpen dl p = createOrOpen' dl p "" Nothing

-- TODO: why not combine open with newDirectoryLayer?
-- then we don't have the clumsiness of always passing a prefix and having
-- to check the allowManualPrefixes bool. Could just pass Maybe Prefix instead.
-- Or is this creating or opening a subdirectory of the input DL?



-- | Open a directory, with optional custom prefix and layer.
-- Returns 'Nothing' if the directory doesn't exist.
open'
  :: DirectoryLayer
  -> Path
  -> ByteString
     -- ^ layer
  -> Maybe ByteString
     -- ^ optional custom prefix
  -> Transaction (Maybe Directory)
open' _  []   _     _       = throwDirUserError CannotOpenRoot
-- TODO: prefix won't be used until we add partition support
open' dl path layer _prefix = do
  checkVersion dl
  find dl path >>= \case
    Nothing -> return Nothing
    Just node -> do
      existingLayer <- getFoundNodeLayer node
      when (layer /= existingLayer)
           (throwDirUserError $ LayerMismatch layer existingLayer)
      Just
        <$> contentsOfNodeSubspace dl
                                   (nodeNodeSS node)
                                   (nodePath node)
                                   existingLayer

-- | Opens a directory at the given path, with optional custom prefix and layer.
createOrOpen'
  :: DirectoryLayer
  -> Path
  -> ByteString
          -- ^ layer
  -> Maybe ByteString
          -- ^ optional custom prefix
  -> Transaction Directory
createOrOpen' _ [] _ _ = throwDirUserError CannotOpenRoot
createOrOpen' dl@DirectoryLayer {..} path layer prefix = do
  tryOpen <- open' dl path layer prefix
  case tryOpen of
    Just ss -> return ss
    Nothing -> do
      prefixToUse <- case prefix of
        Nothing -> do
          newDirPrefix  <- allocate allocator contentSS
          isPrefixEmpty <- isRangeEmpty (subspaceRange newDirPrefix)
          unless isPrefixEmpty
               (throwDirInternalError
                 "Failed to alloc new dir: prefix not empty.")
          isFree <- isPrefixFree dl (pack newDirPrefix [])
          unless isFree
            (throwDirUserError (ManualPrefixConflict $ rawPrefix newDirPrefix))
          return (pack newDirPrefix [])
        Just prefixBytes -> do
          isFree <- isPrefixFree dl prefixBytes
          unless isFree
               (throwDirUserError PrefixInUse)
          return prefixBytes
      parentNode <- if length path > 1
        then do
          pd <- createOrOpen' dl (init path) "" Nothing
          let pdk = subspaceKey (directorySubspace pd)
          return $ nodeWithPrefix dl pdk
        else return rootNode
      --TODO: null check of parentNode here in Go code
      let node = nodeWithPrefix dl prefixToUse
      set
        (pack parentNode [Tuple.Int _SUBDIRS, Tuple.Text (last path)])
        prefixToUse
      set (pack node [Tuple.Bytes "layer"]) layer
      contentsOfNodeSubspace dl node path layer

-- | Returns 'True' iff the given path exists.
exists :: DirectoryLayer
       -> Path
       -> Transaction Bool
exists dl path = isJust <$> find dl path

-- | List the names of the immediate subdirectories of a directory.
-- Returns an empty list if the directory does not exist.
list :: DirectoryLayer -> Path -> Transaction (Seq Text)
list dl path =
  find dl path >>= \case
    Nothing -> return Empty
    Just node -> subdirNames dl (nodeNodeSS node)

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

-- | Move a directory from one path to another.
move
  :: DirectoryLayer
  -> Path
     -- ^ from path
  -> Path
     -- ^ to path
  -> Transaction (Maybe MoveError)
move _  _       []      = return $ Just CannotMoveToRoot
move dl oldPath newPath = do
  let sliceEnd = min (length oldPath) (length newPath)
  if oldPath == take sliceEnd newPath
    then return (Just SelfSubDir)
    else do
      oldNodeM    <- find dl oldPath
      newNodeM    <- find dl newPath
      parentNodeM <- find dl (init newPath)
      case (oldNodeM, newNodeM, parentNodeM) of
        (Nothing, _   , _    ) -> return (Just SourceDoesNotExist)
        (_    , Just _, _    ) -> return (Just DestinationAlreadyExists)
        (_    , _   , Nothing) -> return (Just DestinationParentDoesNotExist)
        (Just oldNode, Nothing, Just parentNode) -> do
          let k = pack
                (nodeNodeSS parentNode)
                [Tuple.Int _SUBDIRS, Tuple.Text (last newPath)]
          let ve =
                unpack (nodeSS dl) (rawPrefix $ nodeNodeSS oldNode)
          case ve of
            Left e -> throwDirInternalError $ "move failure: " ++ e
            Right (Tuple.Bytes v : _) -> do
              set              k  v
              removeFromParent dl oldPath
              return Nothing
            x -> throwDirInternalError $ "move unpack failure: " ++ show x

-- | Remove a directory path, its contents, and all subdirectories.
-- Returns 'True' if removal succeeds. Fails for nonexistent paths and the root
-- directory.
remove
  :: DirectoryLayer
  -> Path
  -> Transaction Bool
-- can't remove root dir
remove _  []   = return False
remove dl path =
  find dl path >>= \case
    Nothing -> return False
    Just node -> do
      removeRecursive  dl (nodeNodeSS node)
      removeFromParent dl path
      return True

-- | Internal helper function that removes all subdirectories of the given
-- node subspace. Does not remove the given node from its parent.
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
    Left  e                        -> throwDirInternalError
                                        $ "removeRecursive: " ++ e
    Right (Tuple.Bytes p' : _) -> case rangeKeys <$> prefixRange p' of
      Just (start, end) -> clearRange start end
      Nothing ->
        throwDirInternalError
          $  "removeRecursive: couldn't make prefix range:"
          ++ show p'
    Right _ -> throwDirInternalError "node unpacked to non-bytes tuple element"
  uncurry clearRange (rangeKeys (subspaceRange node))

-- | Internal helper function that removes a path from its parent. Does not
-- remove the children of the removed path.
removeFromParent :: DirectoryLayer -> Path -> Transaction ()
removeFromParent _  []   = return ()
removeFromParent dl path =
  find dl (init path) >>= \case
    Nothing -> throwDirInternalError $ "parent not found for " ++ show path
    Just (FoundNode sub _ _) ->
      clear (pack sub [Tuple.Int _SUBDIRS, Tuple.Text (last path)])

subdirNameNodes
  :: DirectoryLayer
  -> Subspace
  -- ^ node
  -> Transaction (Seq (Text, Subspace))
subdirNameNodes dl@DirectoryLayer {..} node = do
  let sd = extend node [Tuple.Int _SUBDIRS]
  kvs <- getEntireRange (subspaceRange sd)
  let unpackKV (k, v) = case unpack sd k of
        Right [Tuple.Text t] -> return (t, nodeWithPrefix dl v)
        _ -> throwDirInternalError "failed to unpack node name in subdirNameNodes"
  mapM unpackKV kvs

subdirNames :: DirectoryLayer -> Subspace -> Transaction (Seq Text)
subdirNames dl node = fmap (fmap fst) (subdirNameNodes dl node)

subdirNodes :: DirectoryLayer -> Subspace -> Transaction (Seq Subspace)
subdirNodes dl node = fmap (fmap snd) (subdirNameNodes dl node)

nodeContainingKey
  :: DirectoryLayer -> ByteString -> Transaction (Maybe Subspace)
nodeContainingKey dl@DirectoryLayer {..} k
  | BS.isPrefixOf (pack nodeSS []) k = return $ Just rootNode
  | otherwise = do
    let r = Range
          (rangeBegin $ subspaceRange nodeSS)
          (FirstGreaterOrEq (pack nodeSS [Tuple.Bytes k] <> "\x00"))
          (Just 1)
          True
    rr <- getEntireRange r
    case rr of
      -- TODO: consider dropping [] from RangeDone constructor. Looks like
      -- underlying function is returning RangeMore [x] when there is only one
      -- item in the range.
      Empty       -> return Nothing
      (kv :<| _)  -> processKV kv
 where
  processKV (k', _) = do
    let unpacked = unpack nodeSS k'
    case unpacked of
      Left  _ -> throwDirInternalError "Failed to unpack in nodeContainingKey"
      Right (Tuple.Bytes prevPrefix : _) -> if BS.isPrefixOf prevPrefix k
        then return (Just $ nodeWithPrefix dl prevPrefix)
        else return Nothing
      --TODO: there are two cases where we fail this way. Is this correct?
      -- Is this case actually possible?
      Right _ -> throwDirInternalError "node unpacked to non-bytes element"


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
          let r' = keyRange (pack nodeSS [Tuple.Bytes bk])
                            (pack nodeSS [Tuple.Bytes ek])
          isRangeEmpty r'


checkVersion :: DirectoryLayer -> Transaction ()
checkVersion dl@DirectoryLayer {..} = do
  mver <- get (pack rootNode [Tuple.Bytes "version"]) >>= await
  case mver of
    Nothing       -> initializeDirectory dl
    Just verBytes -> do
      (major, minor, micro) <-
        throwing "Couldn't parse directory version!" $ runGet
          ((,,) <$> getWord32le <*> getWord32le <*> getWord32le)
          verBytes
      when (major > majorVersion)
        $ throwDirUserError
        $ VersionError major minor micro
      when (minor > minorVersion)
        $  throwDirUserError
        $  VersionError major minor micro

initializeDirectory :: DirectoryLayer -> Transaction ()
initializeDirectory DirectoryLayer {..} = do
  let verBytes = runPut $ do
        putWord32le majorVersion
        putWord32le minorVersion
        putWord32le microVersion
  set (pack rootNode [Tuple.Bytes "version"]) verBytes

nodeWithPrefix :: DirectoryLayer -> ByteString -> Subspace
nodeWithPrefix DirectoryLayer {..} prefix =
  extend nodeSS [Tuple.Bytes prefix]

-- | Returns the longest prefix of @path@ that doesn't exist. If the entire
-- path exists, returns it.
find
  :: DirectoryLayer
  -> Path
  -> Transaction (Maybe FoundNode)
find dl@DirectoryLayer {..} queryPath = go baseNode (zip [0 ..] queryPath)
 where

  baseNode = FoundNode rootNode [] queryPath

  go n []            = return (Just n)
  go n ((i, p) : ps) = do
    let nodePrefixKey = pack (nodeNodeSS n)
                             [Tuple.Int _SUBDIRS, Tuple.Text p]
    get nodePrefixKey >>= await >>= \case
      Nothing -> return Nothing
      Just prefix -> do
        let n' = FoundNode (nodeWithPrefix dl prefix)
                           (take (i + 1) queryPath)
                           []
        layer <- getFoundNodeLayer n'
        if layer == "partition" then return (Just n') else go n' ps

contentsOfNodePartition
  :: DirectoryLayer
  -> Subspace
  -- ^ node
  -> Path
  -> Transaction DirPartition
contentsOfNodePartition dl@DirectoryLayer {..} node queryPath = do
  -- TODO: need a type class of things that can be converted to keys to avoid
  -- @pack node []@
  -- TODO: do combinators like throwing already exist in some lib?
  p <- throwing "can't unpack node!" (unpack nodeSS (pack node []))
  case p of
    [Tuple.Bytes prefix] -> do
      let newPath                  = dlPath <> queryPath
      let newDL = newDirectoryLayer
            (subspace [Tuple.Bytes (prefix <> "\xfe")])
            contentSS
            False
      return (DirPartition newDL { dlPath = newPath } dl)
    _ -> throwDirInternalError "unexpected parse in contentsOfNodePartition"

contentsOfNodeSubspace
  :: DirectoryLayer
  -> Subspace
  -> Path
  -> ByteString
  -> Transaction Directory
contentsOfNodeSubspace DirectoryLayer {..} node queryPath layer = do
  -- TODO: need a type class of things that can be converted to keys to avoid
  -- @pack node []@
  p <- throwing "can't unpack node!" (unpack nodeSS (pack node []))
  case p of
    [Tuple.Bytes prefixBytes] -> do
      let newPath = dlPath <> queryPath
      return $ Directory (Subspace prefixBytes) newPath layer
    _ -> throwDirInternalError "unexpected contents for node prefix value"
