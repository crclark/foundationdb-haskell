{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import FoundationDB
import FoundationDB.Layer.Subspace
import FoundationDB.Layer.Tuple

import System.Environment (lookupEnv)
import Test.Hspec

import Properties.FoundationDB.Layer.Tuple (encodeDecodeSpecs, encodeDecodeProps)
import Properties.FoundationDB.Layer.Directory (directorySpecs)
import Properties.FoundationDB.Layer.Subspace (subspaceSpecs)
import Properties.FoundationDB.Transaction

-- | Prefix for all test keys, to reduce the chance of a user accidentally
-- wiping something important.
testSS :: Subspace
testSS = subspace [ BytesElem "foundationdb-haskell-test-"]

cleanup :: Database -> Subspace -> IO ()
cleanup db ss = runTransaction db $ do
  let ssRange = subspaceRange ss
  let (begin,end) = rangeKeys ssRange
  clearRange begin end

main :: IO ()
main = do
  mdbPath <- lookupEnv "FDB_HASKELL_TEST_CLUSTER_FILE"
  case mdbPath of
    Nothing -> error "tests require FDB_HASKELL_TEST_CLUSTER_FILE to be set."
    Just _ -> withFoundationDB currentAPIVersion mdbPath $ \case
      Left e -> error $ "error starting DB: " ++ show e
      Right db -> do
        let cleanupAfter tests = hspec $ after_ (cleanup db testSS) tests
        hspec encodeDecodeSpecs
        hspec encodeDecodeProps
        hspec subspaceSpecs
        cleanupAfter $ transactionProps testSS db
        cleanupAfter $ directorySpecs db testSS
