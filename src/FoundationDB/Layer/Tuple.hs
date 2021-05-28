-- | The tuple layer is the most fundamental layer shared by all language
-- bindings. It is responsible for implementing a key encoding scheme that
-- ensures that many common types (and tuples composed of them) can be written
-- to FoundationDB with a sensible lexicographical ordering. This enables many
-- powerful query patterns. See the
-- <https://apple.github.io/foundationdb/data-modeling.html#data-modeling-tuples official docs>
-- for more information.
module FoundationDB.Layer.Tuple (
  Elem (..)
  , encodeTupleElems
  , encodeTupleElemsWPrefix
  , decodeTupleElems
  , decodeTupleElemsWPrefix
) where

import FoundationDB.Layer.Tuple.Internal
