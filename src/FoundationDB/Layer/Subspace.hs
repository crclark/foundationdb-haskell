module FoundationDB.Layer.Subspace where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid
import Data.Sequence(Seq(Empty,(:<|)))

import FoundationDB
import FoundationDB.Layer.Tuple

-- | Represents a subspace of 'Tuple' keys. A subspace is just a common prefix
-- for a set of tuples.
newtype Subspace = Subspace {rawPrefix :: ByteString}
  deriving (Show, Eq, Ord)

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

subspaceKey :: Subspace -> ByteString
subspaceKey = rawPrefix

-- | Create a subsubspace by extending the prefix of a subspace by the
-- given tuple.
extend :: Subspace
       -> [Elem]
       -> Subspace
extend (Subspace prfx) =
  prefixedSubspace prfx

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

subspaceRange :: Subspace -> Range
subspaceRange s = Range
  { rangeBegin   = FirstGreaterOrEq (k <> BS.pack [0x00])
  , rangeEnd     = FirstGreaterOrEq (k <> BS.pack [0xff])
  , rangeLimit   = Nothing
  , rangeReverse = False
  }
  where k = pack s []

-- | Get the last key,value pair in the subspace, if it exists.
getLast :: Subspace -> Transaction (Maybe (ByteString, ByteString))
getLast sub = do
  rr <- getRange (subspaceRange sub) { rangeLimit = Just 1,
                                       rangeReverse = True
                                     }
  kvs <- await rr
  case kvs of
    RangeDone Empty      -> return Nothing
    RangeDone (kv:<|_)   -> return (Just kv)
    RangeMore (kv:<|_) _ -> return (Just kv)
    RangeMore Empty _    -> return Nothing --TODO: impossible
