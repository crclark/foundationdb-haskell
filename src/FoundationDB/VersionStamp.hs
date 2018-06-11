-- | Version stamps. For a great overview, see
-- https://forums.foundationdb.org/t/implementing-versionstamps-in-bindings/250

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module FoundationDB.VersionStamp where

import Data.ByteString (ByteString)
import Data.Word (Word16, Word64)


data VersionStampCompleteness = Complete | Incomplete

-- | Represents a version stamp. Version stamps consist of
-- * An 8-byte transaction version
-- * A 2-byte transaction batch order
-- * A 2-byte user version
data VersionStamp (a :: VersionStampCompleteness) where
  CompleteVersionStamp :: Word64 -> Word16 -> Word16 -> VersionStamp 'Complete
  -- ^ A complete version stamp, consisting of a transaction version and
  -- transaction batch order set by the database, and a user version set by
  -- the user.
  IncompleteVersionStamp :: Word16 -> VersionStamp 'Incomplete
  -- ^ A version stamp that has not yet been associated with a completed
  -- transaction. Such a version stamp does not yet have an associated
  -- transaction version and transaction batch order, but does have a user
  -- version.

deriving instance Show (VersionStamp a)

deriving instance Eq (VersionStamp a)

deriving instance Ord (VersionStamp a)

transactionVersion :: VersionStamp 'Complete -> Word64
transactionVersion (CompleteVersionStamp x _ _) = x

transactionBatchOrder :: VersionStamp 'Complete -> Word16
transactionBatchOrder (CompleteVersionStamp _ x _) = x

userVersion :: VersionStamp a -> Word16
userVersion (CompleteVersionStamp _ _ x) = x
userVersion (IncompleteVersionStamp x) = x
