{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties.FoundationDB.Layer.Tuple where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import FoundationDB.Error.Internal (Error (Error), FDBHsError (TupleIntTooLarge))
import FoundationDB.Layer.Tuple.Internal
import FoundationDB.Versionstamp
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll)
import Test.QuickCheck.Arbitrary (Arbitrary (..), genericShrink)
import Test.QuickCheck.Gen (Gen, oneof, sized, vectorOf)

instance Arbitrary TransactionVersionstamp where
  arbitrary = TransactionVersionstamp <$> arbitrary <*> arbitrary

instance Arbitrary (Versionstamp 'Complete) where
  arbitrary = CompleteVersionstamp <$> arbitrary <*> arbitrary

instance Arbitrary (Versionstamp 'Incomplete) where
  arbitrary = IncompleteVersionstamp <$> arbitrary

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Elem where
  arbitrary = sized go
    where
      go :: Int -> Gen Elem
      go 0 =
        oneof
          [ return None,
            Bytes <$> arbitrary,
            Text <$> arbitrary,
            Int <$> arbitrary,
            Float <$> arbitrary,
            Double <$> arbitrary,
            Bool <$> arbitrary,
            UUID <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            CompleteVS <$> arbitrary
          ]
      go n = Tuple <$> vectorOf 3 (go (n `div` 8))
  shrink = genericShrink

-- The below example byte strings come from the Python library.
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

exampleEmpty :: ByteString
exampleEmpty = mempty

exampleBytes :: ByteString
exampleBytes = "\x01hello\x00"

exampleUnicodeString :: ByteString
exampleUnicodeString =
  "\x02I\xc3\xb1t\xc3\xabrn\xc3\xa2ti\xc3\xb4n\xc3\xa0li\xc5\xbe\xc3\xa6ti\xc3\xb8n\x00"

exampleNested :: ByteString
exampleNested = "\x05\x15\x01\x00"

exampleNestedNone :: ByteString
exampleNestedNone = "\x05\x00\xff\x00"

examplePosInt :: ByteString
examplePosInt = "\x15\x01"

exampleNegInt :: ByteString
exampleNegInt = BS.pack [19, 250]

exampleZero :: ByteString
exampleZero = BS.pack [20]

exampleLargeInt :: ByteString
exampleLargeInt = "\x1c\x7f\xff\xff\xff\xff\xff\xff\xff"

exampleFloat :: ByteString
exampleFloat = " \xbf\xc0\x00\x00"

exampleDouble :: ByteString
exampleDouble = "!\xbf\xf8\x00\x00\x00\x00\x00\x00"

exampleTrue :: ByteString
exampleTrue = "'"

exampleFalse :: ByteString
exampleFalse = "&"

exampleUUID :: ByteString
exampleUUID =
  BS.pack
    [48, 135, 36, 87, 101, 200, 209, 66, 248, 133, 41, 255, 47, 94, 32, 226, 252]

exampleCompleteVersionstamp :: ByteString
exampleCompleteVersionstamp =
  BS.pack
    [51, 222, 173, 190, 239, 222, 173, 190, 239, 190, 239, 0, 12]

exampleIncompleteVersionstamp :: ByteString
exampleIncompleteVersionstamp =
  BS.pack
    [51, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 12, 1, 0, 0, 0]

exampleTooLargeInt :: Integer
exampleTooLargeInt = 9 * 10 ^ (700 :: Integer)

encodeDecode :: [Elem] -> ByteString -> String -> SpecWith ()
encodeDecode elems encoded desc = do
  it ("encodes " ++ desc) $
    encodeTupleElems elems `shouldBe` encoded
  it ("decodes " ++ desc) $
    decodeTupleElems encoded `shouldBe` Right elems

describeIntegerTypeCodes :: SpecWith ()
describeIntegerTypeCodes = do
  let codes = [0x0c .. 0x1c]
  let byteLengths = [-8 .. 8]
  forM_ (zip codes byteLengths) go
  where
    go (code, 0) = do
      let encoded = encodeTupleElems [Int 0]
      describe "Encoding 0" $
        it
          ("Uses expected code " ++ show code)
          (BS.unpack encoded `shouldBe` [code])
    go (code, byteLength) = do
      let sign = fromIntegral $ signum byteLength
      let numBits = 8 * abs byteLength - 1
      let num = sign * 2 ^ numBits - 1 :: Integer
      let encoded = encodeTupleElems [Int num]
      describe
        ( "Encoding "
            ++ (if sign > 0 then "positive " else "negative ")
            ++ show (abs byteLength)
            ++ "-byte int: "
            ++ show sign
            ++ " * 2^"
            ++ show numBits
            ++ " - 1"
            ++ " = "
            ++ show num
        )
        $ do
          it
            ("Uses expected code " ++ show code)
            (BS.head encoded `shouldBe` code)
          it "Uses correct number of bytes" $
            BS.length encoded `shouldBe` abs byteLength + 1

issue12 :: SpecWith ()
issue12 = describe "Max 8-byte encoded ints" $ do
  it "encodes 2^64 - 1 correctly" $
    encodeTupleElems [Int $ 2 ^ (64 :: Integer) - 1]
      `shouldBe` "\x1c\xff\xff\xff\xff\xff\xff\xff\xff"
  it "encodes - (2^64 - 1) correctly" $
    encodeTupleElems [Int $ negate $ 2 ^ (64 :: Integer) - 1]
      `shouldBe` "\x0c\x00\x00\x00\x00\x00\x00\x00\x00"

decodeInvalidBytes :: SpecWith ()
decodeInvalidBytes = describe "decode invalid bytes" $ do
  let err = Left "could not decode 1 bytes from the end of the bytestring"
  it "decodeTupleElems with invalid tuple" $
    decodeTupleElems "\x50" `shouldBe` err
  it "decodeTupleElemsWPrefix with valid prefix but invalid tuple" $ do
    decodeTupleElemsWPrefix "a" "a\x50" `shouldBe` err

encodeLargeIntegers :: SpecWith ()
encodeLargeIntegers = describe "encode roundtrips with large integers" $ do
  let largeIntegerTuple = [Int 92233720368547758071]
  let negLargeIntegerTuple = [Int (-92233720368547758071)]
  it "roundtrips with large integers" $
    decodeTupleElems (encodeTupleElems largeIntegerTuple)
      `shouldBe` Right largeIntegerTuple
  it "roundtrips with negative large integers" $
    decodeTupleElems (encodeTupleElems negLargeIntegerTuple)
      `shouldBe` Right negLargeIntegerTuple

encodeTooLargeIntegers :: SpecWith ()
encodeTooLargeIntegers = describe "encode with very large integers throws" $ do
  it "throws with large positive integers" $
    (evaluate $ force $ encodeTupleElems [Int exampleTooLargeInt])
      `shouldThrow` (== (Error TupleIntTooLarge))
  it "throws with negative large integers" $
    (evaluate $ force $ encodeTupleElems [Int $ negate exampleTooLargeInt])
      `shouldThrow` (== (Error TupleIntTooLarge))

encodeDecodeSpecs :: SpecWith ()
encodeDecodeSpecs = describe "Tuple encoding" $ do
  encodeDecode [] exampleEmpty "empty tuples"
  encodeDecode [Bytes "hello"] exampleBytes "bytes"
  encodeDecode [Text "Iñtërnâtiônàližætiøn"] exampleUnicodeString "unicode"
  encodeDecode [Tuple [Int 1]] exampleNested "nested tuple"
  encodeDecode [Tuple [None]] exampleNestedNone "nested tuple containing none"
  encodeDecode [Int 1] examplePosInt "positive int"
  encodeDecode [Int (-5)] exampleNegInt "negative int"
  encodeDecode [Int 0] exampleZero "zero"
  encodeDecode [Int (2 ^ (63 :: Int) - 1)] exampleLargeInt "large int"
  encodeDecode [Float 1.5] exampleFloat "float"
  encodeDecode [Double 1.5] exampleDouble "double"
  encodeDecode [Bool True] exampleTrue "True"
  encodeDecode [Bool False] exampleFalse "False"
  -- This is @UUID.fromString "87245765-c8d1-42f8-8529-ff2f5e20e2fc"@
  let (w1, w2, w3, w4) = (2267305829, 3369157368, 2234122031, 1579213564)
  encodeDecode [UUID w1 w2 w3 w4] exampleUUID "UUID"
  let tvs = TransactionVersionstamp 0xdeadbeefdeadbeef 0xbeef
  let vs = CompleteVersionstamp tvs 12
  encodeDecode
    [CompleteVS vs]
    exampleCompleteVersionstamp
    "complete version stamp"
  let ivs = IncompleteVersionstamp 12
  it "encodes incomplete version stamp" $
    encodeTupleElems [IncompleteVS ivs]
      `shouldBe` exampleIncompleteVersionstamp
  -- no encodeDecode for incomplete version stamps because the encoding adds
  -- two bytes at the end that the C FFI bindings remove. The Python code
  -- doesn't roundtrip either.
  describeIntegerTypeCodes
  issue12
  decodeInvalidBytes
  encodeLargeIntegers
  encodeTooLargeIntegers

encodeDecodeProps :: SpecWith ()
encodeDecodeProps = prop "decode . encode == id" $
  forAll arbitrary $ \tuple ->
    Right tuple == decodeTupleElems (encodeTupleElems tuple)
