{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Properties.FoundationDB.Layer.Directory where

import FoundationDB.Layer.Directory.Internal
import FoundationDB
import FoundationDB.Layer.Subspace as SS
import FoundationDB.Layer.Tuple

import Control.Monad (forM_, void)
import qualified Data.Text as T
import GHC.Exts(IsList(..))

import Test.Hspec

directorySpecs :: Database -> Subspace -> SpecWith ()
directorySpecs db testSS = do
  let dl = newDirectoryLayer (SS.extend testSS [IntElem 1])
                             (SS.extend testSS [IntElem 2])
                             False
  describeCreateOrOpen db dl
  describeRemoveFromParent db dl
  describeMove db dl
  describeRemove db dl
  describeList db dl

describeCreateOrOpen :: Database -> DirectoryLayer -> SpecWith ()
describeCreateOrOpen db dl = describe "createOrOpen" $
  it "creates if directory doesn't exist" $ do
    wasCreated <- runTransaction db $ do
      let path = ["my", "cool", "path"]
      void $ createOrOpen' dl path "" Nothing
      exists dl path
    wasCreated `shouldBe` True

describeRemoveFromParent :: Database -> DirectoryLayer -> SpecWith ()
describeRemoveFromParent db dl = describe "removeFromParent" $
  it "removes subdir from dir" $ do
    bs <- runTransaction db $ do
      let path = ["foo", "bar"]
      void $ createOrOpen' dl path "" Nothing
      existedBefore <- exists dl path
      removeFromParent dl path
      existedAfter <- exists dl path
      parentStillExists <- exists dl ["foo"]
      return (existedBefore, existedAfter, parentStillExists)
    bs `shouldBe` (True, False, True)

describeMove :: Database -> DirectoryLayer -> SpecWith ()
describeMove db dl = describe "move" $ do
  it "can move an existing path to a nonexistent one" $ do
    (ret, oldExists, newExists) <- runTransaction db $ do
      let path = ["path1"]
      let newPath = ["path2"]
      void $ createOrOpen' dl path "" Nothing
      ret <- move dl path newPath
      oldExists <- exists dl path
      newExists <- exists dl newPath
      return (ret, oldExists, newExists)
    ret `shouldBe` Nothing
    oldExists `shouldBe` False
    newExists `shouldBe` True

  it "fails if you try to move to an existing dir" $ do
    res <- runTransaction db $ do
      let path = ["path1"]
      let newPath = ["path2"]
      void $ createOrOpen' dl path "" Nothing
      void $ createOrOpen' dl newPath "" Nothing
      move dl path newPath
    res `shouldBe` Just DestinationAlreadyExists

describeRemove :: Database -> DirectoryLayer -> SpecWith ()
describeRemove db dl = describe "remove" $ do
  it "removes existing paths" $ do
    stillExists <- runTransaction db $ do
      let path = ["abc"]
      void $ createOrOpen' dl path "" Nothing
      void $ remove dl path
      exists dl path
    stillExists `shouldBe` False
  it "also removes contents" $ do
    let path = ["foo", "bar"]
    dir <- runTransaction db $ createOrOpen' dl path "" Nothing
    let k = SS.pack (dirSubspace dir) [IntElem 1]
    runTransaction db $ set k "hi"
    success <- runTransaction db $ remove dl path
    success `shouldBe` True
    res <- runTransaction db $ get k >>= await
    res `shouldBe` Nothing

describeList :: Database -> DirectoryLayer -> SpecWith ()
describeList db dl = describe "list" $ do
  it "lists only immediate nodes, not grandchildren" $ do
    res <- runTransaction db $ do
      void $ createOrOpen' dl ["abc", "def", "ghi"] "" Nothing
      void $ createOrOpen' dl ["abc", "foo", "bar"] "" Nothing
      list dl ["abc"]
    res `shouldBe` ["def","foo"]
  it "can list a larger number of subdirectories" $ do
    let subdirs = fromList [T.pack [a] | a <- ['A'..'z']]
    forM_ subdirs $ \subdir -> runTransaction db $
      void $ createOrOpen' dl ["subdirs", subdir] "" Nothing
    res <- runTransaction db $ list dl ["subdirs"]
    res `shouldBe` subdirs

