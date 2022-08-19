{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import FoundationDB
import FoundationDB.Layer.Subspace
import FoundationDB.Layer.Tuple
import Properties.FoundationDB.Layer.Directory (directorySpecs)
import Properties.FoundationDB.Layer.Subspace (subspaceSpecs)
import Properties.FoundationDB.Layer.Tuple (encodeDecodeProps, encodeDecodeSpecs)
import Properties.FoundationDB.Transaction (transactionSpecs)
import Properties.FoundationDB.Versionstamp.Internal (versionstampProps)
import System.Environment (lookupEnv)
import Test.Hspec

-- | Prefix for all test keys, to reduce the chance of a user accidentally
-- wiping something important.
testSS :: Subspace
testSS = subspace [Bytes "foundationdb-haskell-test-"]

cleanup :: Database -> Subspace -> IO ()
cleanup db ss = runTransaction db $ do
  let ssRange = subspaceRangeQuery ss
  let (begin, end) = rangeKeys ssRange
  clearRange begin end

main :: IO ()
main = do
  mv <- lookupEnv "FDB_HASKELL_TEST_API_VERSION" :: IO (Maybe String)
  let version = maybe currentAPIVersion read mv
  withFoundationDB defaultOptions {apiVersion = version} $ \db -> do
    let cleanupAfter tests = hspec $ after_ (cleanup db testSS) tests
    hspec encodeDecodeSpecs
    hspec encodeDecodeProps
    hspec subspaceSpecs
    hspec versionstampProps
    cleanupAfter $ transactionSpecs testSS db

    cleanupAfter $ directorySpecs db testSS
