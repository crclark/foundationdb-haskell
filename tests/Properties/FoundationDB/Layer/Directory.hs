{-# LANGUAGE OverloadedStrings #-}

module Properties.FoundationDB.Layer.Directory where

import FoundationDB.Layer.Directory.Internal
import FoundationDB
import FoundationDB.Layer.Subspace
import qualified FoundationDB.Layer.Tuple as Tuple

import Control.Monad (void)
import Data.ByteString (ByteString)

import Test.Hspec

describeCreateOrOpen :: Database -> DirectoryLayer -> SpecWith ()
describeCreateOrOpen db dl = describe "createOrOpen" $
  it "creates if directory doesn't exist" $ do
    wasCreated <- runTransaction db $ do
      let path = ["my", "cool", "path"]
      void $ createOrOpen dl path "" Nothing
      exists dl path
    wasCreated `shouldBe` True

describeMove :: Database -> DirectoryLayer -> SpecWith ()
describeMove db dl = describe "move" $
  it "can move an existing path to a nonexistent one" $ do
    (oldExists, newExists) <- runTransaction db $ do
      let path = ["path1"]
      let newPath = ["path2"]
      void $ createOrOpen dl path "" Nothing
      void $ move dl path newPath
      oldExists <- exists dl path
      newExists <- exists dl newPath
      return (oldExists, newExists)
    oldExists `shouldBe` False
    newExists `shouldBe` True

describeRemove :: Database -> DirectoryLayer -> SpecWith ()
describeRemove db dl = describe "remove" $
  it "removes existing paths" $ do
    stillExists <- runTransaction db $ do
      let path = ["abc"]
      void $ createOrOpen dl path "" Nothing
      void $ remove dl path
      exists dl path
    stillExists `shouldBe` False

directorySpecs :: Database -> ByteString -> SpecWith ()
directorySpecs db prefix = do
  let dl = newDirectoryLayer (subspace [Tuple.BytesElem prefix]) mempty False
  describeCreateOrOpen db dl
  describeMove db dl
  describeRemove db dl
