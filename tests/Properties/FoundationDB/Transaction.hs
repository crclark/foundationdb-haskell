{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Properties.FoundationDB.Transaction where

import FoundationDB
import FoundationDB.Layer.Tuple
import FoundationDB.Versionstamp

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 ( ByteString )
import Data.Maybe ( fromJust )
import Data.Monoid ((<>))
import Test.Hspec


transactionProps :: ByteString -> Database -> SpecWith ()
transactionProps prefix db = do
    setting prefix db
    cancellation prefix db
    versionstamps prefix db
    readVersions prefix db
    ranges prefix db

setting :: ByteString -> Database -> SpecWith ()
setting prefix db = describe "set and get" $ do

  it "should round trip" $ do
    let k = prefix <> "foo"
    runTransaction db $ set k "bar"
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Just "bar"

  it "returns Nothing after clearing" $ do
    let k = prefix <> "cleartest"
    runTransaction db $ set k "test"
    runTransaction db $ clear k
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Nothing

  it "reads our writes by default" $ do
    let k = prefix <> "onetrans"
    v <- runTransaction db $ do set k "x"
                                get k >>= await
    putStrLn "finished transaction"
    v `shouldBe` Just "x"

cancellation :: ByteString -> Database -> SpecWith ()
cancellation prefix db = describe "transaction cancellation" $ do
  it "should not commit cancelled transactions" $ do
    let k = prefix <> "neverCommitted"
    runTransaction db (set k "test" >> cancel)
      `shouldThrow` (== CError TransactionCanceled)
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Nothing
  it "should not commit writes before a reset" $ do
    let k = prefix <> "neverCommitted"
    let k' = prefix <> "afterReset"
    runTransaction db (set k "test" >> reset >> set k' "test2")
    (v,v') <- runTransaction db ((,) <$> (get k >>= await) <*> (get k' >>= await))
    v `shouldBe` Nothing
    v' `shouldBe` Just "test2"

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

versionstamps :: ByteString -> Database -> SpecWith ()
versionstamps prefix db = describe "versionstamped tuple key" $
  it "versionstamped tuple contains transaction's versionstamp" $ do
    let k = encodeTupleElems
            [BytesElem prefix, IncompleteVSElem (IncompleteVersionstamp 2)]
    let kLower = encodeTupleElems [BytesElem prefix]
    vsFuture <- runTransaction db $ do
      atomicOp SetVersionstampedKey k "hi"
      getVersionstamp
    vs <- join <$> awaitIO vsFuture
    (finalK, v) <- runTransaction db $ do
      finalK <- getKey (FirstGreaterThan kLower) >>= await
      v <- get finalK >>= await
      return (finalK, v)
    vs `shouldSatisfy` isRight
    let Right v = vs
    let (Right [_, CompleteVSElem (CompleteVersionstamp v' _)]) = decodeTupleElems finalK
    v `shouldBe` v'
    runTransaction db (clearRange kLower (kLower <> "\xff"))

readVersions :: ByteString -> Database -> SpecWith ()
readVersions prefix db = describe "Read versions" $
  it "trivial get followed by set gives error" $ do
    res <- runTransaction' db $ do
             v <- getReadVersion >>= await
             setReadVersion v
    res `shouldBe` Left (CError ReadVersionAlreadySet)

ranges :: ByteString -> Database -> SpecWith ()
ranges prefix db = describe "range ops" $ do
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
