{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Properties.FoundationDB.Transaction where

import FoundationDB
import FoundationDB.Error
import FoundationDB.Layer.Subspace as SS
import FoundationDB.Layer.Tuple
import FoundationDB.Options.MutationType (setVersionstampedKey)
import FoundationDB.Transaction (getEntireRange')
import FoundationDB.Versionstamp

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Except ( throwError )
import Control.Monad.IO.Class(liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid((<>))
import Data.Sequence(Seq(Empty))
import qualified Data.Sequence as Seq
import GHC.Exts(IsList(..))
import Test.Hspec


transactionProps :: Subspace -> Database -> SpecWith ()
transactionProps testSS db = do
    setting testSS db
    futures testSS db
    cancellation testSS db
    versionstamps testSS db
#if FDB_API_VERSION > 520
    readVersions testSS db
#endif
    ranges testSS db
    streamingModes testSS db
    retries testSS db
    watches testSS db
    timeouts testSS db

setting :: Subspace -> Database -> SpecWith ()
setting testSS db = describe "set and get" $ do

  it "should be able to get what we set" $ do
    let k = SS.pack testSS [Bytes "foo"]
    runTransaction db $ set k "bar"
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Just "bar"

  it "returns Nothing after clearing" $ do
    let k = SS.pack testSS [Bytes "cleartest"]
    runTransaction db $ set k "test"
    runTransaction db $ clear k
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Nothing

  it "reads our writes by default" $ do
    let k = SS.pack testSS [Bytes "onetrans"]
    v <- runTransaction db $ do set k "x"
                                get k >>= await
    v `shouldBe` Just "x"

  it "awaiting twice returns same result twice" $ do
    let k = SS.pack testSS [Bytes "foo2"]
    runTransaction db $ set k "bar"
    (v1, v2) <- runTransaction db $ do
      f <- get k
      v1 <- await f
      v2 <- await f
      return (v1, v2)
    v1 `shouldBe` v2

futures :: Subspace -> Database -> SpecWith ()
futures testSS db = describe "futures" $ do
  it "has applicative instances" $ do
    let k1 = SS.pack testSS [Bytes "qwerty"]
    let k2 = SS.pack testSS [Bytes "uiop"]
    let v1 = "uiop"
    let v2 = "qwerty"
    runTransaction db $ set k1 v1 >> set k2 v2
    tuple <- runTransaction db $ do
      f1 <- get k1
      f2 <- get k2
      let t = (,,) <$> f1 <*> f2 <*> pure 1
      awaitInterruptible t
    tuple `shouldBe` (Just v1, Just v2, 1)
  it "cancellation" $ do
    let k = SS.pack testSS [Bytes "foo"]
    runTransaction db $ set k "hello"
    result <- runTransaction' db $ do
      f <- get k
      cancelFuture f
      awaitInterruptible f
    result `shouldBe` Left (CError OperationCancelled)
  it "FutureIO" $ do
    let k = SS.pack testSS [Bytes "foo"]
    w1 <- runTransaction db $ watch k
    w2 <- runTransaction db $ watch k
    cancelFutureIO w2
    runTransaction db $ set k "hello"
    v1 <- awaitInterruptibleIO w1
    v1 `shouldBe` Right ()
    v2 <- awaitInterruptibleIO w2
    v2 `shouldBe` Right ()


cancellation :: Subspace -> Database -> SpecWith ()
cancellation testSS db = describe "transaction cancellation" $ do
  it "should not commit cancelled transactions" $ do
    let k = SS.pack testSS [Bytes "neverCommitted"]
    runTransaction db (set k "test" >> cancel)
      `shouldThrow` (== CError TransactionCanceled)
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Nothing
  it "should not commit writes before a reset" $ do
    let k = SS.pack testSS [Bytes "neverCommitted"]
    let k' = SS.pack testSS [Bytes "afterReset"]
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
            [Bytes "vs", IncompleteVS (IncompleteVersionstamp 2)]
    let kLower = SS.pack testSS [Bytes "vs"]
    vsFuture <- runTransaction db $ do
      atomicOp k (setVersionstampedKey "hi")
      getVersionstamp
    vs <- join <$> awaitIO vsFuture
    finalK <- runTransaction db $
      getKey (FirstGreaterThan kLower) >>= await
    vs `shouldSatisfy` isRight
    let Right v = vs
    let (Right [_, CompleteVS (CompleteVersionstamp v' _)]) = SS.unpack testSS finalK
    v `shouldBe` v'

readVersions :: Subspace -> Database -> SpecWith ()
readVersions testSS db = describe "Read versions" $ do
  it "trivial get followed by set gives error" $ do
    res <- runTransaction' db $ do
             v <- getReadVersion >>= await
             setReadVersion v
    res `shouldBe` Left (CError ReadVersionAlreadySet)
  it "monotonically increases" $ do
    readVer1 <- runTransaction db $ getReadVersion >>= await
    vsf <- runTransaction db $ do
      let k = SS.pack testSS [Bytes "monoReadVer"]
      set k "bar"
      getVersionstamp
    Right (Right (TransactionVersionstamp commitVer _)) <- awaitIO vsf
    readVer2 <- runTransaction db $ getReadVersion >>= await
    readVer1 `shouldSatisfy` (< readVer2)
    commitVer `shouldSatisfy` (<= readVer2)
  it "reusing old versions doesn't see newer state" $ do
    let k = SS.pack testSS [Bytes "oldVersionTest"]
    readVer1 <- runTransaction db $ getReadVersion >>= await
    runTransaction db $ set k "bar"
    res <- runTransaction db $ do
      setReadVersion readVer1
      get k >>= await
    res `shouldBe` Nothing

ranges :: Subspace -> Database -> SpecWith ()
ranges testSS db = describe "range ops" $ do
  let rangeSS = SS.extend testSS [Bytes "rangetest"]
  let kvs = fromList [ (SS.pack rangeSS [Bytes $ BS.singleton k], "a")
                     | k <- ['a'..'z']]
  let putKeys = forM_ kvs $ \(k,v) -> runTransaction db $ set k v
  let range = Range (FirstGreaterOrEq (SS.pack rangeSS [Bytes "a"]))
                    (FirstGreaterOrEq (SS.pack rangeSS [Bytes "z\x00"]))
                    Nothing
                    False
  describe "getEntireRange" $
    it "Should return entire range" $ do
      putKeys
      result <- runTransaction db $ getEntireRange range
      result `shouldBe` kvs
  describe "Range selector" $ do
    it "returns reversed range when Reverse=True" $ do
      putKeys
      result <- runTransaction db $ getEntireRange $ range { rangeReverse = True }
      result `shouldBe` Seq.reverse kvs
    it "respects rangeLimit" $ do
      putKeys
      result <- runTransaction db $ getEntireRange range {rangeLimit = Just 5}
      result `shouldBe` Seq.take 5 kvs
    it "respects rangeLimit with reverse" $ do
      putKeys
      result <- runTransaction db $ getEntireRange range { rangeLimit = Just 5
                                                         , rangeReverse = True}
      result `shouldBe` Seq.take 5 (Seq.reverse kvs)
    it "excludes last key when using keyRange" $ do
      putKeys
      let begin = SS.pack rangeSS [Bytes "a"]
      let end = SS.pack rangeSS [Bytes "z"]
      result <- runTransaction db $ getEntireRange (keyRange begin end)
      result `shouldBe` Seq.take (Seq.length kvs - 1) kvs
    it "includes last key when using keyRangeInclusive" $ do
      putKeys
      let begin = SS.pack rangeSS [Bytes "a"]
      let end = SS.pack rangeSS [Bytes "z"]
      result <- runTransaction db $ getEntireRange (keyRangeInclusive begin end)
      result `shouldBe` kvs
    it "excludes first key when using FirstGreaterThan" $ do
      putKeys
      let begin = FirstGreaterThan $ SS.pack rangeSS [Bytes "a"]
      let range' = range {rangeBegin = begin}
      result <- runTransaction db $ getEntireRange range'
      result `shouldBe` Seq.drop 1 kvs
    it "can get inclusive end key by using FirstGreaterThan" $ do
      putKeys
      let end = FirstGreaterThan $ SS.pack rangeSS [Bytes "z"]
      let range' = range {rangeEnd = end}
      result <- runTransaction db $ getEntireRange range'
      result `shouldBe` kvs
    it "returns offset range when using offset" $ do
      putKeys
      let begin = offset 2 $ rangeBegin range
      let range' = range {rangeBegin = begin}
      result <- runTransaction db $ getEntireRange range'
      result `shouldBe` Seq.drop 2 kvs
  describe "isRangeEmpty" $
    it "isRangeEmpty returns expected results" $ do
      (rr, empty1) <- runTransaction db $ do
        empty1 <- isRangeEmpty range
        rr <- getEntireRange range
        return (rr, empty1)
      rr `shouldBe` Empty
      empty1 `shouldBe` True
      putKeys
      empty2 <- runTransaction db $ isRangeEmpty range
      empty2 `shouldBe` False

streamingModes :: Subspace -> Database -> SpecWith ()
streamingModes testSS db = do
  let normalModes = [minBound .. pred StreamingModeExact]
                    <> [succ StreamingModeExact .. maxBound]
  let rangeSS = SS.extend testSS [Bytes "rangetest2"]
  let kvs = fromList @(Seq _) [ (SS.pack rangeSS [Int k], "a")
                              | k <- [1..100]]
  let putKeys = forM_ kvs $ \(k,v) -> runTransaction db $ set k v
  let range = subspaceRange rangeSS
  forM_ normalModes $ \mode -> describe (show mode) $
    it "Works as expected with getEntireRange'" $ do
      putKeys
      rr <- runTransaction db $ getEntireRange' mode range
      length rr `shouldBe` length kvs
  describe "StreamingModeExact" $ do
    it "Throws an exception if range doesn't include limit" $ do
      putKeys
      rr <- runTransaction' db $ getEntireRange' StreamingModeExact range
      rr `shouldBe` Left (CError ExactModeWithoutLimits)
    it "Succeeds when range includes limit" $ do
      putKeys
      rr <- runTransaction db
            $ getEntireRange' StreamingModeExact range{rangeLimit = Just 10}
      length rr `shouldBe` 10

retries :: Subspace -> Database -> SpecWith ()
retries testSS db = describe "retry logic" $ do
  it "eventually bails out" $ do
    res <- runTransaction' db $ do
      void $ throwError $ CError NotCommitted
      set (SS.pack testSS [Bytes "foo"]) "bar"
    res `shouldBe` Left (Error (MaxRetriesExceeded (CError NotCommitted)))
  it "doesn't retry unretryable errors" $ do
    res <- runTransaction' db $ do
      let bigk = BSL.toStrict $
                 BS.toLazyByteString $
                 foldMap BS.int8 $
                 replicate (10^(6 :: Int)) 1
      set bigk "hi"
    res `shouldBe` Left (CError KeyTooLarge)

watches :: Subspace -> Database -> SpecWith ()
watches testSS db = describe "watches" $
  it "doesn't return until key is set" $ do
    mvar <- newEmptyMVar
    let k = SS.pack testSS [Bytes "watchkey"]
    w <- runTransaction db $ watch k
    _ <- forkIO $ awaitIO w >>= putMVar mvar
    beforeSet <- tryTakeMVar mvar
    beforeSet `shouldBe` Nothing
    runTransaction db $ set k "foo"
    afterSet <- takeMVar mvar
    afterSet `shouldBe` Right ()

timeouts :: Subspace -> Database -> SpecWith ()
timeouts testSS db = describe "timeouts" $
  it "aborts after timing out" $ do
    let k = SS.pack testSS [Bytes "timeoutkey"]
    w <- runTransaction' db $ do
      set k "hello"
      liftIO $ threadDelay 1000000
    w `shouldBe` Left (CError TransactionTimedOut)
    v <- runTransaction db $ get k >>= await
    v `shouldBe` Nothing
