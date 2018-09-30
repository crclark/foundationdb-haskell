{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module FoundationDB.Versionstamp.Internal where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Word (Word16, Word64)
import Data.Serialize.Get
import Data.Serialize.Put

-- TODO: everything else spells it Versionstamp, not VersionStamp

data VersionstampCompleteness = Complete | Incomplete

-- | Represents a version stamp. Version stamps consist of
-- * An 8-byte transaction version
-- * A 2-byte transaction batch order
-- * A 2-byte user version
data Versionstamp (a :: VersionstampCompleteness) where
  -- | A complete version stamp, consisting of a transaction version and
  -- transaction batch order set by the database, and a user version set by
  -- the user.
  CompleteVersionstamp :: Word64 -> Word16 -> Word16 -> Versionstamp 'Complete
  -- | A version stamp that has not yet been associated with a completed
  -- transaction. Such a version stamp does not yet have an associated
  -- transaction version and transaction batch order, but does have a user
  -- version.
  IncompleteVersionstamp :: Word16 -> Versionstamp 'Incomplete


deriving instance Show (Versionstamp a)

deriving instance Eq (Versionstamp a)

deriving instance Ord (Versionstamp a)

putVersionstamp :: Putter (Versionstamp a)
putVersionstamp (CompleteVersionstamp tv tb uv) = do
  putWord64be tv
  putWord16be tb
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

getVersionstampComplete :: Get (Versionstamp 'Complete)
getVersionstampComplete = do
  tv <- getWord64be
  bo <- getWord16be
  uv <- getWord16be
  guard $ not $ tv == maxBound && bo == maxBound
  return $ CompleteVersionstamp tv bo uv

-- | Decode a versionstamp from a raw bytestring. You probably don't need this;
-- see the facilities in "FoundationDB.Layer.Tuple" for a more flexible
-- alternative.
decodeVersionstamp :: ByteString -> Maybe (Versionstamp 'Complete)
decodeVersionstamp bs = case runGet getVersionstampComplete bs of
  Left _ -> Nothing
  Right x -> Just x
