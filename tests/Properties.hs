{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import FoundationDB
import FoundationDB.Layer.Subspace
import FoundationDB.Layer.Tuple

import Test.Hspec

import Properties.FoundationDB.Layer.Tuple (encodeDecodeSpecs, encodeDecodeProps)
import Properties.FoundationDB.Layer.Directory (directorySpecs)
import Properties.FoundationDB.Layer.Subspace (subspaceSpecs)
import Properties.FoundationDB.Transaction

-- | Prefix for all test keys, to reduce the chance of a user accidentally
-- wiping something important.
testSS :: Subspace
testSS = subspace [ Bytes "foundationdb-haskell-test-"]

cleanup :: Database -> Subspace -> IO ()
cleanup db ss = runTransaction db $ do
  let ssRange = subspaceRange ss
  let (begin,end) = rangeKeys ssRange
  clearRange begin end

main :: IO ()
main =
  withFoundationDB defaultOptions $ \ db -> do
    let cleanupAfter tests = hspec $ after_ (cleanup db testSS) tests
    hspec encodeDecodeSpecs
    hspec encodeDecodeProps
    hspec subspaceSpecs
    cleanupAfter $ transactionProps testSS db
    cleanupAfter $ directorySpecs db testSS
