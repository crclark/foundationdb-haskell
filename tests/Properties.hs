{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import FoundationDB.Experimental

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import System.Environment (lookupEnv)
import Test.Hspec

-- | Prefix for all test keys, to reduce the chance of a user accidentally
-- wiping something important.
prefix :: ByteString
prefix = "foundationdb-haskell-test-"

cleanup :: Database -> IO ()
cleanup db = runTransaction db $ do
  let begin = prefix <> "a"
  let end = prefix <> "z"
  fut <- getRange $ Range { rangeBegin = FirstGreaterOrEq begin
                          , rangeEnd = LastLessOrEq end
                          , rangeLimit = Nothing
                          , rangeReverse = False
                          }
  res <- await fut
  go res

  where go (RangeDone kvs) = forM_ (map fst kvs) clear
        go (RangeMore kvs more) = do
          forM_ (map fst kvs) clear
          res <- await more
          go res

main :: IO ()
main = withFoundationDB $ do
  mdbPath <- lookupEnv "FDB_HASKELL_TEST_CLUSTER_FILE"
  case mdbPath of
    Nothing -> error "tests require FDB_HASKELL_TEST_CLUSTER_FILE to be set."
    Just _ -> withDatabase mdbPath $ \case
      Left e -> error $ "error starting DB: " ++ show e
      Right db -> do
        hspec $ after_ (cleanup db) $ do
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
