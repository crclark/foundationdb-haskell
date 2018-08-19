{-# LANGUAGE OverloadedStrings #-}

module FoundationDB.Layer.Directory.Internal.Node where

import Data.ByteString (ByteString)
import Data.Text (Text)

import FoundationDB
import FoundationDB.Layer.Directory.Internal.Error
import FoundationDB.Layer.Subspace
import FoundationDB.Layer.Tuple

data Node = Node
  { nodeNodeSS :: Maybe Subspace
  , nodePath :: [Text]
  , targetPath :: [Text]
  } deriving (Show, Eq, Ord)

-- TODO: it's not clear yet how this is used. It's possible that this should be
-- a sum type, or that we shouldn't even have a 'Node' if it doesn't actually
-- exist.
nodeExists :: Node -> Bool
nodeExists (Node Nothing _ _) = False
nodeExists (Node (Just _) _ _) = True

getNodeLayer :: Node -> Transaction ByteString
getNodeLayer (Node (Just ss) _ _) = do
  fv <- get (pack ss [BytesElem "layer"]) >>= await
  case fv of
    Nothing -> throwDirError "Failed to get node layer."
    Just l -> return l
getNodeLayer (Node Nothing _ _) =
  throwDirError "Tried to get layer of nonexistent node."
