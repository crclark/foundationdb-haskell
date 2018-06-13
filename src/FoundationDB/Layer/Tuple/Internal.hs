{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module FoundationDB.Layer.Tuple.Internal where

import FoundationDB.VersionStamp

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans (lift)
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as A
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Serialize.Get
import qualified Data.Serialize.IEEE754 as Put
import Data.Serialize.IEEE754 (getFloat32be, getFloat64be)
import qualified Data.Serialize.Put as Put
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Integer.Logarithms
import Numeric.Search.Range (searchFromTo)

-- | Crude UUID type to avoid dependency on UUID library. Interconvertible with
-- @toWords@ and @fromWords@ in 'Data.UUID'.
data UUID = UUID Word32 Word32 Word32 Word32
  deriving (Show, Eq, Ord)

-- | Elements of tuples
data Elem =
  NoneElem
  | BytesElem ByteString
  | TextElem T.Text
  | TupleElem [Elem]
  | IntElem Int
  | FloatElem Float
  | DoubleElem Double
  | BoolElem Bool
  | UUIDElem UUID
  | CompleteVSElem (VersionStamp 'Complete)
  | IncompleteVSElem (VersionStamp 'Incomplete)

deriving instance Show Elem
deriving instance Ord Elem
deriving instance Eq Elem
deriving instance Generic Elem

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
negStartCode = 0x0b
zeroCode = 0x14
posEndCode = 0x1d
floatCode = 0x20
doubleCode = 0x21
falseCode = 0x26
trueCode = 0x27
uuidCode = 0x30
versionstampCode = 0x33

data SerializationState = SerializationState
  { currLength :: Int
  , incompleteVersionStampPos :: Maybe Int
  } deriving (Show, Eq, Ord)

newtype PutTuple a =
  PutTuple {unPutTuple :: StateT SerializationState Put.PutM a}
  deriving (Functor, Applicative, Monad)

deriving instance MonadState SerializationState PutTuple

-- | returns the serialized tuple and the position of the incomplete version
-- stamp, if any.
runPutTuple :: PutTuple () -> (ByteString, Maybe Int)
runPutTuple x =
  let (((), s), bs) = Put.runPutM $
                      runStateT (unPutTuple x) (SerializationState 0 Nothing)
      in case incompleteVersionStampPos s of
        Nothing -> (bs, Nothing)
        Just i -> (bs <> Put.runPut (Put.putWord16le (fromIntegral i)), Just i)

incrLength :: Int -> PutTuple ()
incrLength !i = do
  s <- get
  put s {currLength = currLength s + i}

liftPutM :: Put.PutM a -> PutTuple a
liftPutM x = PutTuple $ lift x

putWord8 :: Word8 -> PutTuple ()
putWord8 x = do
  liftPutM (Put.putWord8 x)
  incrLength 1

putWord16be :: Word16 -> PutTuple ()
putWord16be x = do
  liftPutM (Put.putWord16be x)
  incrLength 2

putWord32be :: Word32 -> PutTuple ()
putWord32be x = do
  liftPutM (Put.putWord32be x)
  incrLength 4

putWord64be :: Word64 -> PutTuple ()
putWord64be x = do
  liftPutM (Put.putWord64be x)
  incrLength 8

putByteString :: ByteString -> PutTuple ()
putByteString bs = do
  liftPutM (Put.putByteString bs)
  incrLength (BS.length bs)

encodeBytes :: ByteString -> PutTuple ()
encodeBytes bs = mapM_ f (BS.unpack bs) >> putWord8 0x00
  where f 0x00 = putWord8 0x00 >> putWord8 0xff
        f x = putWord8 x

-- @truncatedInt n v@ returns the last n bytes of v, encoded big endian.
truncatedInt :: Int -> Int -> ByteString
truncatedInt n v = BS.drop (8-n) (Put.runPut (Put.putWord64be $ fromIntegral v))

encodePosInt :: Int -> PutTuple ()
encodePosInt v =
  if v >= sizeLimits A.! 7
    then do let l = fromIntegral (bitLen v + 7 `div` 8)
            putWord8 posEndCode
            putWord8 l
            forM_ [l-1,l-2..0] $ \i ->
              putWord8 ((fromIntegral v `shiftR` fromIntegral (8*i)) .&. 0xff)
    else do let n = bisectSize v
            putWord8 (zeroCode + fromIntegral n)
            putByteString $ truncatedInt n v

encodeNegInt :: Int -> PutTuple ()
encodeNegInt v =
  if negate v >= sizeLimits A.! 7
    then do let l = fromIntegral (bitLen v + 7 `div` 8)
            let v' = fromIntegral $ v + (1 `shiftL` fromIntegral (8*l)) - 1
            putWord8 negStartCode
            putWord8 (l `xor` 0xff)
            forM_ [l-1,l-2..0] $ \i ->
              putWord8 ((v' `shiftR` fromIntegral (8*i)) .&. 0xff)
    else do let n = bisectSize (negate v)
            let maxv = sizeLimits A.! n
            putWord8 (zeroCode - fromIntegral n)
            putByteString $ truncatedInt n (maxv + v)

-- | given an IEEE 754 float/double, adjust it for encoding.
floatAdjust :: Bool -> ByteString -> ByteString
floatAdjust isEncode bs
  | isEncode && (BS.head bs .&. 0x80) /= 0 = BS.map (xor 0xff) bs
  | not isEncode && (BS.head bs .&. 0x80) /= 0x80 = BS.map (xor 0xff) bs
  | otherwise = BS.cons (BS.head bs `xor` 0x80) (BS.tail bs)

encodeElem :: Bool
           -- ^ Whether we are inside a nested tuple
           -> Elem
           -- ^ elem to encode
           -> PutTuple ()
encodeElem True NoneElem =
  putWord8 nullCode >> putWord8 0xff
encodeElem False NoneElem =
  putWord8 nullCode
encodeElem _ (BytesElem bs) =
  putWord8 bytesCode >> encodeBytes bs
encodeElem _ (TextElem t) =
  putWord8 stringCode >> encodeBytes (encodeUtf8 t)
encodeElem _ (IntElem 0) = putWord8 zeroCode
encodeElem _ (IntElem n) = if n > 0 then encodePosInt n else encodeNegInt n
encodeElem _ (FloatElem x) = do
  putWord8 floatCode
  putByteString $ floatAdjust True $ Put.runPut $ Put.putFloat32be x
encodeElem _ (DoubleElem x) = do
  putWord8 doubleCode
  putByteString $ floatAdjust True $ Put.runPut $ Put.putFloat64be x
encodeElem _ (BoolElem True) = putWord8 trueCode
encodeElem _ (BoolElem False) = putWord8 falseCode
encodeElem _ (UUIDElem (UUID w x y z)) = do
  putWord8 uuidCode
  putWord32be w
  putWord32be x
  putWord32be y
  putWord32be z
encodeElem _ (TupleElem xs) = do
  putWord8 nestedCode
  mapM_ (encodeElem True) xs
  putWord8 0x00
encodeElem _ (CompleteVSElem (CompleteVersionStamp tv tb uv)) = do
  putWord8 versionstampCode
  putWord64be tv
  putWord16be tb
  putWord16be uv
encodeElem _ (IncompleteVSElem (IncompleteVersionStamp uv)) = do
  putWord8 versionstampCode
  s <- get
  put s{incompleteVersionStampPos = Just $ currLength s}
  putWord64be maxBound
  putWord16be maxBound
  putWord16be uv

-- | Encodes a tuple from a list of tuple elements. Returns the encoded
-- tuple and the index of the incomplete version stamp, if an incomplete
-- version stamp was included in the input.
--
-- Note that this encodes to the format expected by FoundationDB as input, which
-- is slightly different from the format returned by FoundationDB as output. The
-- difference is that if the encoded bytes include an incomplete version stamp,
-- two bytes are appended to the end to indicate the index of the incomplete
-- version stamp so that FoundationDB can fill in the transaction version and
-- batch order.
encodeTupleElems :: [Elem] -> (ByteString, Maybe Int)
encodeTupleElems = runPutTuple . mapM_ (encodeElem False)

encodeTupleElems' :: [Elem] -> ByteString
encodeTupleElems' = fst . runPutTuple . mapM_ (encodeElem False)

decodeTupleElems :: ByteString -> Either String [Elem]
decodeTupleElems = runGet $ many (decodeElem False)

decodeElem :: Bool -> Get Elem
decodeElem nested =
  decodeNoneElem nested
  <|> decodeBytesElem
  <|> decodeTextElem
  <|> decodeIntElem
  <|> decodeFloatElem
  <|> decodeDoubleElem
  <|> decodeBoolElem
  <|> decodeUUIDElem
  <|> decodeTupleElem
  <|> decodeVersionStamp

expectCode :: Word8 -> Get ()
expectCode c = do
  c' <- getWord8
  guard (c == c')

decodeNoneElem :: Bool -> Get Elem
decodeNoneElem nested = do
  expectCode nullCode
  when nested (expectCode 0xff)
  return NoneElem

bytesTerminator :: Get ()
bytesTerminator = do
  n <- remaining
  guard (n == 0)
  <|> do c <- getWord8
         n <- remaining
         guard (c == 0x00 && n == 0)
  <|> do c <- getWord8
         d <- lookAhead getWord8
         guard (c == 0x00 && d /= 0xff)

-- | Returns number of bytes remaining until terminator. Does not consume
-- terminator.
remainingUntil :: Get () -> Get Int
remainingUntil term = lookAhead (go 0)
  where go !i =
          (lookAhead term >> return i)
          <|>
          (getWord8 >> go (i+1))

-- | Takes all bytes up to terminator. Consumes and discards terminator.
getBytesUntil :: Get () -> Get ByteString
getBytesUntil term = do
  n <- remainingUntil term
  bs <- getBytes n
  term
  return bs

decodeBytes :: ByteString -> ByteString
decodeBytes bs =
  BS.pack $ go $ BS.unpack bs
  where go [] = []
        go [x] = [x]
        go (0x00:0xff:xs) = 0x00 : go xs
        go (x:xs) = x : go xs

decodeBytesElem :: Get Elem
decodeBytesElem = do
  expectCode bytesCode
  bs <- getBytesUntil bytesTerminator
  return $ BytesElem $ decodeBytes bs

decodeTextElem :: Get Elem
decodeTextElem = do
  expectCode stringCode
  bs <- getBytesUntil bytesTerminator
  return $ TextElem $ decodeUtf8 $ decodeBytes bs

decodeSmallPosInt :: Get Elem
decodeSmallPosInt = do
  code <- getWord8
  guard (code >= zeroCode && code < posEndCode)
  let n = fromIntegral $ code - 20
  bs <- (BS.pack (replicate (8 - n) 0x00) <>) <$> getBytes n
  let subres = runGet getWord64be bs
  case subres of
    Left e -> fail e
    Right x -> return $ IntElem $ fromIntegral x

decodeSmallNegInt :: Get Elem
decodeSmallNegInt = do
  code <- getWord8
  guard (code > negStartCode && code < zeroCode)
  let n = fromIntegral $ 20 - code
  bs <- (BS.pack (replicate (8 - n) 0x00) <>) <$> getBytes n
  let subres = runGet getWord64be bs
  case subres of
    Left e -> fail e
    Right x -> return $ IntElem $ fromIntegral x - sizeLimits A.! n

decodeLargeNegInt :: Get Elem
decodeLargeNegInt = do
  expectCode negStartCode
  (n :: Int) <- fromIntegral . xor 0xff <$> getWord8
  go 0 n 0

  where go i n x | i == n = return $ IntElem x
                 | otherwise = do d <- fromIntegral <$> getWord8
                                  go (i+1) n (d + (x `shiftL` 8))

decodeLargePosInt :: Get Elem
decodeLargePosInt = do
  expectCode posEndCode
  (n :: Int) <- fromIntegral <$> getWord8
  go 0 n 0

  where go i n x | i == n = return $ IntElem x
                 | otherwise = do d <- fromIntegral <$> getWord8
                                  go (i+1) n (d + (x `shiftL` 8))

decodeIntElem :: Get Elem
decodeIntElem =
  decodeSmallPosInt
  <|> decodeLargePosInt
  <|> decodeSmallNegInt
  <|> decodeLargeNegInt

decodeFloatElem :: Get Elem
decodeFloatElem = do
  expectCode floatCode
  fBytes <- floatAdjust False <$> getByteString 4
  let subres = runGet getFloat32be fBytes
  case subres of
    Left e -> fail e
    Right x -> return $ FloatElem x

decodeDoubleElem :: Get Elem
decodeDoubleElem = do
  expectCode doubleCode
  fBytes <- floatAdjust False <$> getByteString 8
  let subres = runGet getFloat64be fBytes
  case subres of
    Left e -> fail e
    Right x -> return $ DoubleElem x

decodeBoolElem :: Get Elem
decodeBoolElem = do
  c <- getWord8
  guard (c == falseCode || c == trueCode)
  return (BoolElem (c == trueCode))

decodeUUIDElem :: Get Elem
decodeUUIDElem = do
  expectCode uuidCode
  UUIDElem <$> (UUID <$> getWord32be
                     <*> getWord32be
                     <*> getWord32be
                     <*> getWord32be)

decodeTupleElem :: Get Elem
decodeTupleElem = do
  expectCode nestedCode
  ts <- loop
  _ <- getWord8 -- 0x00 terminator
  return (TupleElem ts)

  where
    loop = do
      isEnd <- checkEnd
      if isEnd
        then return []
        else do next <- decodeElem True
                rest <- loop
                return (next : rest)

    checkEnd :: Get Bool
    checkEnd = do
      c <- lookAhead getWord8
      if c == 0x00
        then lookAhead ((/= 0xff) <$> getWord8) <|> return True
        else return False

decodeVersionStamp :: Get Elem
decodeVersionStamp = do
  expectCode versionstampCode
  tv <- getWord64be
  bo <- getWord16be
  uv <- getWord16be
  if tv == maxBound && bo == maxBound
    then return $ IncompleteVSElem $ IncompleteVersionStamp uv
    else return $ CompleteVSElem $ CompleteVersionStamp tv bo uv
