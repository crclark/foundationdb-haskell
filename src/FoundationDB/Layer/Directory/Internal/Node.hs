{-# LANGUAGE OverloadedStrings #-}

module FoundationDB.Layer.Directory.Internal.Node where

import Data.ByteString (ByteString)
import Data.Text (Text)

import FoundationDB
import FoundationDB.Layer.Directory.Internal.Error
import FoundationDB.Layer.Subspace
import FoundationDB.Layer.Tuple

-- | Represents the result of searching for a node with 'find'.
data FoundNode = FoundNode
  { nodeNodeSS :: Subspace
  , nodePath :: [Text]
  , targetPath :: [Text]
  } deriving (Show, Eq, Ord)

getFoundNodeLayer :: FoundNode -> Transaction ByteString
getFoundNodeLayer n@(FoundNode ss _ _) = do
  fv <- get (pack ss [Bytes "layer"]) >>= await
  case fv of
    Nothing -> throwDirInternalError
                 $ "Failed to get node layer for node " ++ show n
    Just l -> return l
