{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Properties.FoundationDB.Transaction where

import FoundationDB
import FoundationDB.Error
import FoundationDB.Layer.Subspace as SS
import FoundationDB.Layer.Tuple
import FoundationDB.Versionstamp

import Control.Exception
import Control.Monad
import Control.Monad.Except ( throwError )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 ( ByteString )
import Data.Maybe ( fromJust )
import Data.Monoid ((<>))
import Test.Hspec


transactionProps :: Subspace -> Database -> SpecWith ()
transactionProps testSS db = do
    setting testSS db
    cancellation testSS db
    versionstamps testSS db
    readVersions testSS db
    ranges testSS db
    retries testSS db

setting :: Subspace -> Database -> SpecWith ()
setting testSS db = describe "set and get" $ do

  it "should round trip" $ do
    let k = SS.pack testSS [BytesElem "foo"]
    runTransaction db $ set k "bar"
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Just "bar"

  it "returns Nothing after clearing" $ do
    let k = SS.pack testSS [BytesElem "cleartest"]
    runTransaction db $ set k "test"
    runTransaction db $ clear k
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Nothing

  it "reads our writes by default" $ do
    let k = SS.pack testSS [BytesElem "onetrans"]
    v <- runTransaction db $ do set k "x"
                                get k >>= await
    putStrLn "finished transaction"
    v `shouldBe` Just "x"

cancellation :: Subspace -> Database -> SpecWith ()
cancellation testSS db = describe "transaction cancellation" $ do
  it "should not commit cancelled transactions" $ do
    let k = SS.pack testSS [BytesElem "neverCommitted"]
    runTransaction db (set k "test" >> cancel)
      `shouldThrow` (== CError TransactionCanceled)
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Nothing
  it "should not commit writes before a reset" $ do
    let k = SS.pack testSS [BytesElem "neverCommitted"]
    let k' = SS.pack testSS [BytesElem "afterReset"]
    runTransaction db (set k "test" >> reset >> set k' "test2")
    (v,v') <- runTransaction db ((,) <$> (get k >>= await) <*> (get k' >>= await))
    v `shouldBe` Nothing
    v' `shouldBe` Just "test2"

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

versionstamps :: Subspace -> Database -> SpecWith ()
versionstamps testSS db = describe "versionstamped tuple key" $
  it "versionstamped tuple contains transaction's versionstamp" $ do
    let k = SS.pack testSS
            [BytesElem "vs", IncompleteVSElem (IncompleteVersionstamp 2)]
    let kLower = SS.pack testSS [BytesElem "vs"]
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
    let (Right [_, CompleteVSElem (CompleteVersionstamp v' _)]) = SS.unpack testSS finalK
    v `shouldBe` v'

readVersions :: Subspace -> Database -> SpecWith ()
readVersions _testSS db = describe "Read versions" $
  it "trivial get followed by set gives error" $ do
    res <- runTransaction' db $ do
             v <- getReadVersion >>= await
             setReadVersion v
    res `shouldBe` Left (CError ReadVersionAlreadySet)

ranges :: Subspace -> Database -> SpecWith ()
ranges testSS db = describe "range ops" $ do
  let kvs = [ (SS.pack testSS [BytesElem $ BS.singleton k], "a")
            | k <- ['a'..'z']]
  describe "unlimited range" $
    it "Should return entire range" $ do
      forM_ kvs $ \(k,v) -> runTransaction db $ set k v
      let unlim = Range (FirstGreaterOrEq (SS.pack testSS [BytesElem "a"]))
                        (FirstGreaterOrEq (SS.pack testSS [BytesElem "z\x00"]))
                        Nothing
                        False
      result <- runTransaction db $ getEntireRange unlim
      result `shouldBe` kvs

retries :: Subspace -> Database -> SpecWith ()
retries testSS db = describe "retry logic" $ do
  it "eventually bails out" $ do
    res <- runTransaction' db $ do
      throwError $ CError NotCommitted
      set (SS.pack testSS [BytesElem "foo"]) "bar"
    res `shouldBe` Left (Error (MaxRetriesExceeded (CError NotCommitted)))
  it "doesn't retry unretryable errors" $ do
    res <- runTransaction' db $ do
      let bigk = BSL.toStrict $
                 BS.toLazyByteString $
                 foldMap BS.int8 $ replicate 10000000 1
      set bigk "hi"
    res `shouldBe` Left (CError KeyTooLarge)
