-- | Versionstamps. For a great overview, see
-- https://forums.foundationdb.org/t/implementing-versionstamps-in-bindings/250

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module FoundationDB.Versionstamp (
  Versionstamp (..)
  , VersionstampCompleteness (..)
  , TransactionVersionstamp (..)
  , encodeVersionstamp
  , decodeVersionstamp
  , decodeTransactionVersionstamp
  , transactionVersion
  , transactionBatchOrder
  , userVersion
) where

import Data.Word (Word16, Word64)

import FoundationDB.Versionstamp.Internal

transactionVersion :: TransactionVersionstamp -> Word64
transactionVersion (TransactionVersionstamp x _) = x

transactionBatchOrder :: TransactionVersionstamp -> Word16
transactionBatchOrder (TransactionVersionstamp _ x) = x

userVersion :: Versionstamp a -> Word16
userVersion (CompleteVersionstamp _ x) = x
userVersion (IncompleteVersionstamp x) = x
