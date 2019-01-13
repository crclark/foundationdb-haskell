{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module FoundationDB.Versionstamp.Internal where

import Data.ByteString (ByteString)
import Data.Word (Word16, Word64)
import Data.Persist ( Persist(..)
                    , Put
                    , putBE
                    , runPut
                    , Get
                    , runGet
                    , BigEndian(..))

data VersionstampCompleteness = Complete | Incomplete

-- | Represents a version stamp. Version stamps consist of
-- * An 8-byte transaction version
-- * A 2-byte transaction batch order
-- * A 2-byte user version
data Versionstamp (a :: VersionstampCompleteness) where
  -- | A complete version stamp, consisting of 'TransactionVersionstamp', and a
  -- user version set by the user.
  CompleteVersionstamp :: TransactionVersionstamp
                       -> Word16
                       -> Versionstamp 'Complete
  -- | A version stamp that has not yet been associated with a completed
  -- transaction. Such a version stamp does not yet have an associated
  -- transaction version and transaction batch order, but does have a user
  -- version.
  IncompleteVersionstamp :: Word16 -> Versionstamp 'Incomplete


deriving instance Show (Versionstamp a)

deriving instance Eq (Versionstamp a)

deriving instance Ord (Versionstamp a)

-- | A 'TransactionVersionstamp' consists of a monotonically-increasing
-- 8-byte transaction version and a 2-byte transaction batch order. Each
-- transaction has an associated 'TransactionVersionstamp'.
data TransactionVersionstamp = TransactionVersionstamp Word64 Word16
  deriving (Show, Eq, Ord)

putTransactionVersionstamp :: TransactionVersionstamp -> Put ()
putTransactionVersionstamp (TransactionVersionstamp tv tb) = do
  putBE tv
  putBE tb

encodeTransactionVersionstamp :: TransactionVersionstamp -> ByteString
encodeTransactionVersionstamp = runPut . putTransactionVersionstamp

putVersionstamp :: Versionstamp a -> Put ()
putVersionstamp (CompleteVersionstamp tvs uv) = do
  putTransactionVersionstamp tvs
  putBE uv
putVersionstamp (IncompleteVersionstamp uv) = do
  putBE (maxBound :: Word64)
  putBE (maxBound :: Word16)
  putBE uv

-- | Encodes a versionstamp into a bytestring. You probably don't need this;
-- see the facilities in "FoundationDB.Layer.Tuple" for a more flexible
-- alternative.
encodeVersionstamp :: Versionstamp a -> ByteString
encodeVersionstamp = runPut . putVersionstamp

getTransactionVersionstamp :: Get TransactionVersionstamp
getTransactionVersionstamp =
  TransactionVersionstamp
  <$> fmap unBE get
  <*> fmap unBE get

getVersionstampComplete :: Get (Versionstamp 'Complete)
getVersionstampComplete =
  CompleteVersionstamp <$> getTransactionVersionstamp <*> fmap unBE get

-- | Decode a versionstamp from a raw bytestring. You probably don't need this;
-- see the facilities in "FoundationDB.Layer.Tuple" for a more flexible
-- alternative.
decodeVersionstamp :: ByteString -> Maybe (Versionstamp 'Complete)
decodeVersionstamp bs = case runGet getVersionstampComplete bs of
  Left _ -> Nothing
  Right x -> Just x

decodeTransactionVersionstamp :: ByteString -> Maybe TransactionVersionstamp
decodeTransactionVersionstamp bs = case runGet getTransactionVersionstamp bs of
  Left _ -> Nothing
  Right x -> Just x
