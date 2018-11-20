{-# LANGUAGE OverloadedStrings #-}

module Properties.FoundationDB.Layer.Directory where

import FoundationDB.Layer.Directory.Internal
import FoundationDB
import FoundationDB.Layer.Subspace

import Control.Monad (void)
import Data.ByteString (ByteString)

import Test.Hspec

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
describeRemove db dl = describe "remove" $
  it "removes existing paths" $ do
    stillExists <- runTransaction db $ do
      let path = ["abc"]
      void $ createOrOpen' dl path "" Nothing
      void $ remove dl path
      exists dl path
    stillExists `shouldBe` False

describeList :: Database -> DirectoryLayer -> SpecWith ()
describeList db dl = describe "list" $
  it "lists only immediate nodes, not grandchildren" $ do
    res <- runTransaction db $ do
      void $ createOrOpen' dl ["abc", "def", "ghi"] "" Nothing
      void $ createOrOpen' dl ["abc", "foo", "bar"] "" Nothing
      list dl ["abc"]
    res `shouldBe` ["def","foo"]

directorySpecs :: Database -> ByteString -> SpecWith ()
directorySpecs db prefix = do
  let dl = newDirectoryLayer (Subspace prefix)
                             (Subspace "")
                             False
  describeCreateOrOpen db dl
  describeRemoveFromParent db dl
  describeMove db dl
  describeRemove db dl
  describeList db dl
