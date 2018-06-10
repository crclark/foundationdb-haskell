{-# LANGUAGE MagicHash #-}

module FoundationDB.Layer.Tuple where

import Control.Monad
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as A
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Serialize
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8, Word32)
import GHC.Exts
import GHC.Integer.Logarithms
import Numeric.Search.Range (searchFromTo)

data Elem =
  TEmpty
  | TBytes ByteString
  | TText T.Text
  | TTuple [Elem]
  | TInt Int
  | TFloat Float
  | TDouble Double
  | TBool Bool
  | TUUID Word32 Word32 Word32 Word32
  deriving (Show, Eq, Ord)

sizeLimits :: Array Int Int
sizeLimits = A.listArray (0,7) [shiftL 1 (i*8) - 1 | i <- [0..7]]

-- | Returns smallest size limit greater than input.
bisectSize :: Int -> Int
bisectSize n = fromMaybe 8 $ searchFromTo (\x -> (sizeLimits A.! x) > n) 0 7

-- | Returns the minimum number of bits needed to encode the given int.
bitLen :: Integral a => a -> Int
bitLen x = 1 + I# (integerLog2# (fromIntegral x))

nullCode, bytesCode, stringCode, nestedCode :: Word8
zeroCode, posEndCode, negStartCode, floatCode :: Word8
doubleCode, falseCode, trueCode, uuidCode, versionstampCode :: Word8

nullCode = 0x00
bytesCode = 0x01
stringCode = 0x02
nestedCode = 0x05
zeroCode = 0x14
posEndCode = 0x1d
negStartCode = 0x0b
floatCode = 0x20
doubleCode = 0x21
falseCode = 0x26
trueCode = 0x27
uuidCode = 0x30
versionstampCode = 0x33

encodeBytes :: ByteString -> Put
encodeBytes bs = mapM_ f (BS.unpack bs) >> putWord8 0x00
  where f 0x00 = putWord8 0x00 >> putWord8 0xff
        f x = putWord8 x

-- @truncatedInt n v@ returns the last n bytes of v, encoded big endian.
truncatedInt :: Int -> Int -> ByteString
truncatedInt n v = BS.drop (8-n) (runPut (putWord64be $ fromIntegral v))

encodePosInt :: Int -> Put
encodePosInt v =
  if v >= sizeLimits A.! 7
    then do let l = fromIntegral (bitLen v + 7 `div` 8)
            putWord8 posEndCode
            putWord8 l
            forM_ [l-1,l-2..0] $ \i ->
              putWord8 ((fromIntegral v `shiftR` (fromIntegral $ 8*i)) .&. 0xff)
    else do let n = bisectSize v
            putWord8 (zeroCode + fromIntegral n)
            putByteString $ truncatedInt n v

encodeNegInt :: Int -> Put
encodeNegInt v =
  if (negate v) >= sizeLimits A.! 7
    then do let l = fromIntegral (bitLen v + 7 `div` 8)
            let v' = fromIntegral $ v + (1 `shiftL` (fromIntegral $ 8*l)) - 1
            putWord8 negStartCode
            putWord8 (l `xor` 0xff)
            forM_ [l-1,l-2..0] $ \i ->
              putWord8 ((v' `shiftR` (fromIntegral $ 8*i)) .&. 0xff)
    else do let n = bisectSize (negate v)
            let maxv = sizeLimits A.! n
            putWord8 (zeroCode - fromIntegral n)
            putByteString $ truncatedInt n (maxv + v)

-- | given an IEEE 754 float/double, adjust it for encoding.
floatAdjust :: Bool -> ByteString -> ByteString
floatAdjust encode bs =
  if encode && (BS.head bs .&. 0x80) /= 0
    then BS.map (xor 0xff) bs
    else if not encode && (BS.head bs .&. 0x80) /= 0x80
      then BS.map (xor 0xff) bs
      else BS.cons (BS.head bs `xor` 0x80) (BS.tail bs)

encodeElem :: Bool
           -- ^ Whether we are inside a nested tuple
           -> Elem
           -- ^ elem to encode
           -> Put
encodeElem True TEmpty =
  putWord8 nullCode >> putWord8 0xff
encodeElem False TEmpty =
  putWord8 nullCode
encodeElem _ (TBytes bs) =
  putWord8 bytesCode >> encodeBytes bs
encodeElem _ (TText t) =
  putWord8 stringCode >> encodeBytes (encodeUtf8 t)
encodeElem _ (TInt 0) = putWord8 zeroCode
encodeElem _ (TInt n) = if n > 0 then encodePosInt n else encodeNegInt n
encodeElem _ (TFloat x) = do
  putWord8 floatCode
  putByteString $ floatAdjust True $ runPut $ putFloat32be x
encodeElem _ (TDouble x) = do
  putWord8 doubleCode
  putByteString $ floatAdjust True $ runPut $ putFloat64be x
encodeElem _ (TBool True) = putWord8 trueCode
encodeElem _ (TBool False) = putWord8 falseCode
encodeElem _ (TUUID w x y z) = do
  putWord8 uuidCode
  putWord32be w
  putWord32be x
  putWord32be y
  putWord32be z
encodeElem _ (TTuple xs) = do
  putWord8 nestedCode
  mapM_ (encodeElem True) xs
  putWord8 0x00

encodeTuple :: [Elem] -> ByteString
encodeTuple = runPut . mapM_ (encodeElem False)
