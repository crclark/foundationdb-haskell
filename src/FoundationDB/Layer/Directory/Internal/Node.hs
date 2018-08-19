{-# LANGUAGE OverloadedStrings #-}

module FoundationDB.Layer.Directory.Internal.Node where

import Data.ByteString (ByteString)
import Data.Text (Text)

import FoundationDB
import FoundationDB.Layer.Directory.Internal.Error
import FoundationDB.Layer.Subspace
import FoundationDB.Layer.Tuple

data Node = Node
  { nodeNodeSS :: Subspace
  , nodePath :: [Text]
  , targetPath :: [Text]
  } deriving (Show, Eq, Ord)

getNodeLayer :: Node -> Transaction ByteString
getNodeLayer n@(Node ss _ _) = do
  fv <- get (pack ss [BytesElem "layer"]) >>= await
  case fv of
    Nothing -> throwDirError $ "Failed to get node layer for node " ++ show n
    Just l -> return l
