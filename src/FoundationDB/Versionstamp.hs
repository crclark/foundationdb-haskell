-- | Versionstamps. For a great overview, see
-- https://forums.foundationdb.org/t/implementing-versionstamps-in-bindings/250

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module FoundationDB.Versionstamp (
  Versionstamp (..)
  , VersionstampCompleteness (..)
  , encodeVersionstamp
  , decodeVersionstamp
  , transactionVersion
  , transactionBatchOrder
  , userVersion
) where

import Data.Word (Word16, Word64)

import FoundationDB.Versionstamp.Internal

transactionVersion :: Versionstamp 'Complete -> Word64
transactionVersion (CompleteVersionstamp x _ _) = x

transactionBatchOrder :: Versionstamp 'Complete -> Word16
transactionBatchOrder (CompleteVersionstamp _ x _) = x

userVersion :: Versionstamp a -> Word16
userVersion (CompleteVersionstamp _ _ x) = x
userVersion (IncompleteVersionstamp x) = x
