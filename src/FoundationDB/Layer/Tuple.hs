module FoundationDB.Layer.Tuple where

import Data.ByteString.Char8 (ByteString)

class FDBTuple a where
  encode :: a -> ByteString

