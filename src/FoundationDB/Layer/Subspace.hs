{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FoundationDB.Layer.Subspace where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import FoundationDB.Layer.Tuple

-- | Represents a subspace of keys. sub-subspaces can be created with the Monoid
-- instance: @mySubsubpace = mySubspace <> subspace someTuple@.
newtype Subspace = Subspace {rawPrefix :: ByteString}
  deriving (Show, Eq, Ord, Monoid)

-- | Create a subspace from a tuple.
subspace :: [Elem]
         -- ^ Tuple with which to prefix the subspace. May not contain
         -- incomplete version stamps.
         -> Subspace
subspace es = Subspace (encodeTupleElems es)

-- | Create a subspace from a raw bytestring prefix and a tuple.
prefixedSubspace :: ByteString
                 -- ^ prefix
                 -> [Elem]
                 -> Subspace
prefixedSubspace prefix tuple = Subspace (encodeTupleElemsWPrefix prefix tuple)

pack :: Subspace -> [Elem] -> ByteString
pack sub = encodeTupleElemsWPrefix (rawPrefix sub)

unpack :: Subspace -> ByteString -> Either String [Elem]
unpack sub = decodeTupleElemsWPrefix (rawPrefix sub)

-- | Returns 'True' iff the subspace contains the given key.
contains :: Subspace
         -> ByteString
         -- ^ encoded key
         -> Bool
contains sub = BS.isPrefixOf (rawPrefix sub)

