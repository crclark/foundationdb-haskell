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
main = withFoundationDB currentAPIVersion $ do
  mdbPath <- lookupEnv "FDB_HASKELL_TEST_CLUSTER_FILE"
  case mdbPath of
    Nothing -> error "tests require FDB_HASKELL_TEST_CLUSTER_FILE to be set."
    Just _ -> withDatabase mdbPath $ \case
      Left e -> error $ "error starting DB: " ++ show e
      Right db -> do
        hspec encodeDecodeSpecs
        hspec encodeDecodeProps
        hspec subspaceSpecs
        hspec $ after_ (cleanup db prefix) $ do
          describe "set and get" $ do

            it "should round trip" $ do
              let k = prefix <> "foo"
              runTransaction db $ set k "bar"
              v <- runTransaction db $ do f <- get k
                                          await f
              v `shouldBe` Just "bar"

            it "returns Nothing after clearing" $ do
              let k = prefix <> "cleartest"
              runTransaction db $ set k "test"
              runTransaction db $ clear k
              v <- runTransaction db $ get k >>= await
              v `shouldBe` Nothing

            it "round trips when both commands are in one transaction" $ do
              let k = prefix <> "onetrans"
              v <- runTransaction db $ do set k "x"
                                          get k >>= await
              putStrLn "finished transaction"
              v `shouldBe` Just "x"

          describe "transaction cancellation" $
            it "should not commit cancelled transactions" $ do
              let k = prefix <> "neverCommitted"
              runTransaction db (set k "test" >> cancel)
                `shouldThrow` (== CError TransactionCanceled)
              v <- runTransaction db $ get k >>= await
              v `shouldBe` Nothing

          describe "versionstamped tuple key" $
            it "can set and get keys containing version stamps" $ do
              let k = encodeTupleElems
                      [IntElem 2, IncompleteVSElem (IncompleteVersionstamp 2)]
              let kLower = encodeTupleElems [IntElem 2]
              runTransaction db (atomicOp SetVersionstampedKey k "hi")
              (finalK, v) <- runTransaction db $ do
                finalK <- getKey (FirstGreaterThan kLower) >>= await
                v <- get finalK >>= await
                return (finalK, v)
              let matches (Right [IntElem 2, CompleteVSElem _]) = True
                  matches _ = False
              decodeTupleElems finalK `shouldSatisfy` matches
              v `shouldBe` Just "hi"
              runTransaction db (clear finalK)

          describe "Read versions" $
            it "trivial get followed by set gives error" $ do
              res <- runTransaction' db $ do
                       v <- getReadVersion >>= await
                       setReadVersion v
              res `shouldBe` Left (CError ReadVersionAlreadySet)

          rangeSpec db

        let dirSpecPrfx = "fdb-haskell-dir"
        hspec $ after_ (cleanup db dirSpecPrfx) $ directorySpecs db dirSpecPrfx

rangeSpec :: Database -> SpecWith ()
rangeSpec db = do
  let kvs = [(prefix <> BS.singleton k,"a") | k <- ['a'..'z']]
  describe "unlimited range" $
    it "Should return entire range" $ do
      forM_ kvs $ \(k,v) -> runTransaction db $ set k v
      let unlim = Range (FirstGreaterOrEq (prefix <> "a"))
                        (FirstGreaterOrEq (prefix <> "z\x00"))
                        Nothing
                        False
      result <- runTransaction db $ getEntireRange unlim
      result `shouldBe` kvs
