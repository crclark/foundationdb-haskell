{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module FoundationDB.Versionstamp.Internal where

import Control.DeepSeq (NFData(..))
import Data.ByteString (ByteString)
import Data.Word (Word16, Word64)
import Data.Serialize.Get
import Data.Serialize.Put
import GHC.Generics (Generic)

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

instance Bounded (Versionstamp 'Complete) where
  minBound = CompleteVersionstamp minBound minBound
  maxBound = CompleteVersionstamp maxBound maxBound

instance NFData (Versionstamp a) where
  rnf (CompleteVersionstamp tv w) = rnf tv `seq` w `seq` ()
  rnf (IncompleteVersionstamp w) = w `seq` ()

-- | A 'TransactionVersionstamp' consists of a monotonically-increasing
-- 8-byte transaction version and a 2-byte transaction batch order. Each
-- transaction has an associated 'TransactionVersionstamp'.
data TransactionVersionstamp = TransactionVersionstamp Word64 Word16
  deriving (Show, Read, Eq, Ord, Generic, NFData, Bounded)

putTransactionVersionstamp :: Putter TransactionVersionstamp
putTransactionVersionstamp (TransactionVersionstamp tv tb) = do
  putWord64be tv
  putWord16be tb

encodeTransactionVersionstamp :: TransactionVersionstamp -> ByteString
encodeTransactionVersionstamp = runPut . putTransactionVersionstamp

putVersionstamp :: Putter (Versionstamp a)
putVersionstamp (CompleteVersionstamp tvs uv) = do
  putTransactionVersionstamp tvs
  putWord16be uv
putVersionstamp (IncompleteVersionstamp uv) = do
  putWord64be maxBound
  putWord16be maxBound
  putWord16be uv

-- | Encodes a versionstamp into a bytestring. You probably don't need this;
-- see the facilities in "FoundationDB.Layer.Tuple" for a more flexible
-- alternative.
encodeVersionstamp :: Versionstamp a -> ByteString
encodeVersionstamp = runPut . putVersionstamp

getTransactionVersionstamp :: Get TransactionVersionstamp
getTransactionVersionstamp =
  TransactionVersionstamp <$> getWord64be <*> getWord16be

getVersionstampComplete :: Get (Versionstamp 'Complete)
getVersionstampComplete =
  CompleteVersionstamp <$> getTransactionVersionstamp <*> getWord16be

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
