-- | Subspaces allow you to easily attach a common prefix to tuple-encoded keys
-- (See "FoundationDB.Layer.Tuple") so that you can perform range reads and
-- deletes over a set of related keys.
--
-- The subspace layer is one of the standard layers supported by all
-- language bindings. See
-- <https://apple.github.io/foundationdb/developer-guide.html#subspaces the official documentation>
-- for more information.
module FoundationDB.Layer.Subspace
  ( -- * Creating subspaces
    Subspace (..),
    subspace,
    prefixedSubspace,

    -- * Using subspaces
    extend,
    pack,
    unpack,
    contains,
    subspaceRangeQuery,
    getLast,
    subspaceKey,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Sequence (Seq (Empty, (:<|)))
import FoundationDB
import FoundationDB.Layer.Tuple

-- | Represents a subspace of 'Tuple' keys. A subspace is just a common prefix
-- for a set of tuples.
newtype Subspace = Subspace {rawPrefix :: ByteString}
  deriving (Show, Eq, Ord)

-- | Create a subspace from a tuple.
subspace ::
  -- | Tuple with which to prefix the subspace. May not contain
  -- incomplete version stamps.
  [Elem] ->
  Subspace
subspace es = Subspace (encodeTupleElems es)

-- | Create a subspace from a raw bytestring prefix and a tuple.
prefixedSubspace ::
  -- | prefix
  ByteString ->
  [Elem] ->
  Subspace
prefixedSubspace prefix tuple = Subspace (encodeTupleElemsWPrefix prefix tuple)

-- | Returns the bytestring prefix of the subspace. Equivalent to 'rawPrefix'.
-- This function is provided for consistency with other language bindings.
subspaceKey :: Subspace -> ByteString
subspaceKey = rawPrefix

-- | Create a subsubspace by extending the prefix of a subspace by the
-- given tuple.
extend ::
  Subspace ->
  [Elem] ->
  Subspace
extend (Subspace prfx) =
  prefixedSubspace prfx

-- | Encode a tuple prefixed with a subspace.
pack :: Subspace -> [Elem] -> ByteString
pack sub = encodeTupleElemsWPrefix (rawPrefix sub)

-- | Decode a tuple that was encoded by 'pack'.
unpack :: Subspace -> ByteString -> Either String [Elem]
unpack sub = decodeTupleElemsWPrefix (rawPrefix sub)

-- | Returns 'True' iff the subspace contains the given key.
contains ::
  Subspace ->
  -- | encoded key
  ByteString ->
  Bool
contains sub = BS.isPrefixOf (rawPrefix sub)

-- | Construct a range query that covers an entire subspace.
subspaceRangeQuery :: Subspace -> RangeQuery
subspaceRangeQuery s =
  RangeQuery
    { rangeBegin = FirstGreaterOrEq (k <> BS.pack [0x00]),
      rangeEnd = FirstGreaterOrEq (k <> BS.pack [0xff]),
      rangeLimit = Nothing,
      rangeReverse = False
    }
  where
    k = pack s []

-- | Get the last key,value pair in the subspace, if it exists.
getLast :: Subspace -> Transaction (Maybe (ByteString, ByteString))
getLast sub = do
  rr <-
    getRange
      (subspaceRangeQuery sub)
        { rangeLimit = Just 1,
          rangeReverse = True
        }
  kvs <- await rr
  case kvs of
    RangeDone Empty -> return Nothing
    RangeDone (kv :<| _) -> return (Just kv)
    RangeMore (kv :<| _) _ -> return (Just kv)
    RangeMore Empty _ -> return Nothing --NOTE: impossible case
