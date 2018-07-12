{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FoundationDB.Layer.Directory where

import Data.ByteString (ByteString)
import Data.Monoid

import FoundationDB
import FoundationDB.Layer.Directory.Internal.HCA
import FoundationDB.Layer.Subspace
import qualified FoundationDB.Layer.Tuple as Tuple

data DirectoryLayer = DirectoryLayer
  { nodeSS :: Subspace
    -- ^ Subspace for directory metadata.
  , contentSS :: Subspace
    -- ^ Subspace for directory content.
  , allocator :: HCA
  , rootNode :: Subspace
  -- TODO: allowManualPrefixes is only used in createOrOpen. Eliminate?
  , allowManualPrefixes :: Bool
  , path :: [Tuple.Elem]
  } deriving (Show, Eq, Ord)

newDirectoryLayer :: Subspace
                  -- ^ node subspace for directory metadata
                  -> Subspace
                  -- ^ content subspace
                  -> Bool
                  -- ^ allow manual prefixes
                  -> DirectoryLayer
newDirectoryLayer nodeSS contentSS allowManualPrefixes =
  let rootNode = nodeSS
      allocator = newHCA (rootNode <> subspace [Tuple.BytesElem "hca"])
      path = []
      in DirectoryLayer{..}

defaultDirLayer :: DirectoryLayer
defaultDirLayer =
  newDirectoryLayer (subspace [Tuple.BytesElem "\xfe"])
                    mempty
                    False

createOrOpen :: DirectoryLayer -> [ByteString] -> Transaction Subspace
createOrOpen = undefined

exists :: DirectoryLayer -> [ByteString] -> Transaction Bool
exists = undefined

list :: DirectoryLayer -> [ByteString] -> Transaction [ByteString]
list = undefined

move :: DirectoryLayer
     -> [ByteString]
     -- ^ from path
     -> [ByteString]
     -- ^ to path
     -> Transaction [ByteString]
move = undefined

remove :: DirectoryLayer
       -> [ByteString]
       -> Transaction Bool
remove = undefined

removeRecursive :: DirectoryLayer
                -> [ByteString]
                -> Transaction ()
removeRecursive = undefined

subdirNames :: DirectoryLayer
            -> Subspace
            -- ^ node
            -> Transaction [ByteString]
subdirNames = undefined

subdirNodes :: DirectoryLayer
            -> Subspace
            -- ^ node
            -> Transaction [Subspace]
subdirNodes = undefined

nodeContainingKey :: DirectoryLayer
                  -> ByteString
                  -> Transaction Subspace
nodeContainingKey = undefined

isPrefixFree :: DirectoryLayer
             -> ByteString
             -> Transaction Bool
isPrefixFree = undefined

checkVersion :: DirectoryLayer
             -> Transaction ()
checkVersion = undefined

initializeDirectory :: DirectoryLayer
                    -> Transaction ()
initializeDirectory = undefined

contentsOfNode :: DirectoryLayer
               -> Subspace
               -- ^ node
               -> [ByteString]
               -- ^ path
               -> ByteString
               -- ^ layer
               -> Transaction Subspace
contentsOfNode = undefined

find :: DirectoryLayer
     -> [ByteString]
     -- ^ path
     -> Transaction Subspace
find = undefined
