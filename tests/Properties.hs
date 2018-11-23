{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import FoundationDB
import FoundationDB.Layer.Tuple
import FoundationDB.Versionstamp

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Monoid ((<>))
import System.Environment (lookupEnv)
import Test.Hspec

import Properties.FoundationDB.Layer.Tuple (encodeDecodeSpecs, encodeDecodeProps)
import Properties.FoundationDB.Layer.Directory (directorySpecs)
import Properties.FoundationDB.Layer.Subspace (subspaceSpecs)
import Properties.FoundationDB.Transaction

-- | Prefix for all test keys, to reduce the chance of a user accidentally
-- wiping something important.
prefix :: ByteString
prefix = "foundationdb-haskell-test-"

cleanup :: Database -> ByteString -> IO ()
cleanup db prfx = runTransaction db $ do
  let begin = prfx
  let end = prfx <> "\xff"
  clearRange begin end

main :: IO ()
main = do
  mdbPath <- lookupEnv "FDB_HASKELL_TEST_CLUSTER_FILE"
  case mdbPath of
    Nothing -> error "tests require FDB_HASKELL_TEST_CLUSTER_FILE to be set."
    Just _ -> withFoundationDB currentAPIVersion mdbPath $ \case
      Left e -> error $ "error starting DB: " ++ show e
      Right db -> do
        let cleanupAfter tests = hspec $ after_ (cleanup db prefix) tests
        hspec encodeDecodeSpecs
        hspec encodeDecodeProps
        hspec subspaceSpecs
        cleanupAfter $ transactionProps prefix db
        cleanupAfter $ directorySpecs db "fdb-haskell-dir-prefix"
