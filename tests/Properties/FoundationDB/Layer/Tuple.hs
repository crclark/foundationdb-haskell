{-# LANGUAGE OverloadedStrings #-}

module Properties.FoundationDB.Layer.Tuple where

import FoundationDB.Layer.Tuple.Internal
import FoundationDB.VersionStamp

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Data.UUID as UUID
import Test.Hspec

-- NOTE: bytestring string literals do not follow the same rules as Python byte
-- array literals. In Python, @b'\xbeef'@ is 3 bytes long-- it's 0xbe, 'e', 'f'.
-- However, Haskell's IsString parses the literal as a Haskell string first and
-- then feeds it to ByteString's IsString instance, but Haskell's strings are
-- unicode strings. Thus, @"\xbeef"@ is considered to be one character, 0xbeef.
-- The ByteString code truncates that to a single byte, resulting in a one-byte
-- ByteString containing 0xef. Copying/pasting a Python byte array literal to
-- Haskell will only work if it just so happens that literals won't be parsed
-- as containing hex numbers more than two digits long. The
-- "Iñtërnâtiônàližætiøn" test case below was lucky in that way. The UUID
-- test case was not, because it contains 0xd1 followed by 'B', which gets
-- parsed as 0xd1b by Haskell, which then gets truncated to 0x1b. To avoid
-- terrible confusion when comparing to the Python library, copy/paste the
-- output of @[x for x in fdb.tuple.pack((True,))]@ rather than of
-- @fdb.tuple.pack((True,))@.

encodeSpecs :: SpecWith ()
encodeSpecs = describe "Tuple encoding" $ do
  it "encodes empty tuples correctly" $ do
    encodeTupleElems' [] `shouldBe` mempty
  it "encodes bytes" $
    encodeTupleElems' [TBytes "hello"] `shouldBe` "\x01hello\x00"
  it "encodes unicode" $
    encodeTupleElems' [TText "Iñtërnâtiônàližætiøn"]
    `shouldBe`
    "\x02I\xc3\xb1t\xc3\xabrn\xc3\xa2ti\xc3\xb4n\xc3\xa0li\xc5\xbe\xc3\xa6ti\xc3\xb8n\x00"
  it "encodes nested tuple" $
    encodeTupleElems' [TTuple [TInt 1]] `shouldBe` "\x05\x15\x01\x00"
  it "encodes positive int" $
    encodeTupleElems' [TInt 1] `shouldBe` "\x15\x01"
  it "encodes negative int" $
    encodeTupleElems' [TInt (-5)] `shouldBe` BS.pack [19, 250]
  it "encodes zero" $
    encodeTupleElems' [TInt 0] `shouldBe` BS.pack [20]
  it "encodes larger int" $
    encodeTupleElems' [TInt 123456789] `shouldBe` BS.pack [24, 7, 91, 205, 21]
  it "encodes float" $
    encodeTupleElems' [TFloat 1.5] `shouldBe` " \xbf\xc0\x00\x00"
  it "encodes double" $
    encodeTupleElems' [TDouble 1.5] `shouldBe` "!\xbf\xf8\x00\x00\x00\x00\x00\x00"
  it "encodes bool" $ do
    encodeTupleElems' [TBool True] `shouldBe` "'"
    encodeTupleElems' [TBool False] `shouldBe` "&"
  it "encodes UUID" $ do
    let uuid = fromJust $ UUID.fromString "87245765-c8d1-42f8-8529-ff2f5e20e2fc"
    let (w1,w2,w3,w4) = UUID.toWords uuid
    let encoded = BS.pack [48, 135, 36, 87, 101, 200, 209, 66, 248, 133, 41, 255, 47, 94, 32, 226, 252]
    encodeTupleElems' [TUUID w1 w2 w3 w4] `shouldBe` encoded
  it "encodes complete version stamp" $ do
    let vs = CompleteVersionStamp 0xdeadbeefdeadbeef 0xbeef 12
    let encoded = BS.pack [51, 222, 173, 190, 239, 222, 173, 190, 239, 190, 239, 0, 12]
    encodeTupleElems' [TCVersionStamp vs] `shouldBe` encoded
  it "encodes incomplete version stamp" $ do
    let vs = IncompleteVersionStamp 12
    let encoded = BS.pack[51, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 12, 1, 0]
    encodeTupleElems [TIVersionStamp vs] `shouldBe` (encoded, Just 1)
