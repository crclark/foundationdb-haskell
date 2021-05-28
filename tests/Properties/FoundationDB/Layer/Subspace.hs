{-# LANGUAGE OverloadedStrings #-}

module Properties.FoundationDB.Layer.Subspace where

import qualified Data.ByteString as BS

import Test.Hspec

import FoundationDB.Layer.Subspace
import FoundationDB.Layer.Tuple

subspaceSpecs :: SpecWith ()
subspaceSpecs = describe "subspaces" $ do
  let sb = subspace [Bytes "hello", Bytes "world"]
  it "packing empty tuple gives same output as .key() in python" $
    pack sb [] `shouldBe` BS.pack [1, 104, 101, 108, 108, 111, 0, 1,
                                   119, 111, 114, 108, 100, 0]
  it "packing a tuple is equivalent to creating the entire tuple" $
    pack sb [Int 12] `shouldBe` encodeTupleElems [ Bytes "hello"
                                                     , Bytes "world"
                                                     , Int 12]

  it "subsubspaces give same result as python" $ do
    let nodeSubspace = Subspace "\xfe"
    let rootNode = extend nodeSubspace [Bytes (subspaceKey nodeSubspace)]
    BS.unpack (pack rootNode []) `shouldBe` [254,1,254,0]

  it "packing tuple with raw prefix subspace gives same result as python" $ do
    let nodeSubspace = Subspace "\xfe"
    let rootNode = extend nodeSubspace [Bytes (subspaceKey nodeSubspace)]
    let k = subspaceKey $ extend rootNode [Int 0]
    BS.unpack k `shouldBe` [254, 1, 254, 0, 20]
    let k' = pack rootNode [Int 0, Bytes "helloworld"]
    BS.unpack k' `shouldBe`
      [254, 1, 254, 0, 20, 1, 104, 101, 108, 108, 111, 119, 111, 114, 108, 100, 0]

  it "key packed in subspace is contained by subspace" $ do
    let k = pack sb [Int 0]
    contains sb k `shouldBe` True
