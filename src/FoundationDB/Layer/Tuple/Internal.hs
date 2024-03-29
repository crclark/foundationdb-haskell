{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module FoundationDB.Layer.Tuple.Internal where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Exception (throw)
import Control.Monad
import Control.Monad.State.Strict
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as A
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize.Get
  ( Get,
    getByteString,
    getBytes,
    getWord16be,
    getWord32be,
    getWord64be,
    getWord8,
    lookAhead,
    remaining,
    runGet,
    runGetState,
  )
import Data.Serialize.IEEE754 (getFloat32be, getFloat64be)
import qualified Data.Serialize.IEEE754 as Put
import qualified Data.Serialize.Put as Put
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Tuple (swap)
import Data.Word (Word16, Word32, Word64, Word8)
import FoundationDB.Error.Internal (Error (Error), FDBHsError (TupleIntTooLarge))
import FoundationDB.Versionstamp hiding (decodeVersionstamp)
import GHC.Exts (Int (I#))
import GHC.Generics (Generic)
import GHC.Integer.Logarithms (integerLog2#)

-- | Elements of tuples. A tuple is represented as a list of these. Note that
-- a tuple may contain at most one incomplete version stamp. Future versions of
-- this library may introduce a more strongly typed tuple representation that
-- enforces this restriction.
data Elem
  = -- | Corresponds to null or nil types in other language bindings.
    None
  | -- | Nested tuples.
    Tuple [Elem]
  | Bytes ByteString
  | Text T.Text
  | -- | Variable-length integer encodings. For values that fit within a 64-bit
    -- signed integer, the <https://github.com/apple/foundationdb/blob/master/design/tuple.md#integer standard integer>
    -- encoding is used. For larger values, the <https://github.com/apple/foundationdb/blob/master/design/tuple.md#positive-arbitrary-precision-integer provisional spec>
    -- for Java and Python values is used.
    Int Integer
  | Float Float
  | Double Double
  | Bool Bool
  | -- | Crude UUID to avoid dependency on UUID library. Interconvertible with
    -- @toWords@ and @fromWords@ in 'Data.UUID'.
    UUID Word32 Word32 Word32 Word32
  | CompleteVS (Versionstamp 'Complete)
  | -- | This constructor is to be used in conjunction with 'encodeTupleElems' and
    -- the 'setVersionstampedKey' atomic operation. See 'encodeTupleElems' for
    -- more information.
    IncompleteVS (Versionstamp 'Incomplete)
  deriving (Show, Eq, Ord, Generic)

instance NFData Elem

sizeLimits :: Array Int Integer
sizeLimits = A.listArray (0, 8) [shiftL 1 (i * 8) - 1 | i <- [0 .. 8]]

-- | Returns smallest size limit greater than input.
bisectSize :: Integer -> Int
bisectSize n = go 0
  where
    go 8 = 8
    go !i = if n < (sizeLimits A.! i) then i else go (i + 1)

-- | Returns the minimum number of bits needed to encode the given int.
bitLen :: Integer -> Int
bitLen x = 1 + I# (integerLog2# (abs x))

-- | Returns the minimum number of bytes needed to encode the given int.
byteLen :: Integer -> Int
byteLen x = (bitLen x + 7) `div` 8

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
  { currLength :: Int,
    incompleteVersionstampPos :: Maybe Int
  }
  deriving (Show, Eq, Ord)

newtype PutTuple a = PutTuple {unPutTuple :: StateT SerializationState Put.PutM a}
  deriving (Functor, Applicative, Monad)

deriving instance MonadState SerializationState PutTuple

-- | returns the serialized tuple and the position of the incomplete version
-- stamp, if any.
runPutTuple :: PutTuple () -> (ByteString, Maybe Int)
runPutTuple x = swap $
  Put.runPutM $ do
    ((), s) <- runStateT (unPutTuple x) (SerializationState 0 Nothing)
    forM_ (incompleteVersionstampPos s) $ \i -> Put.putWord32le (fromIntegral i)
    return (incompleteVersionstampPos s)

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
  where
    f 0x00 = putWord8 0x00 >> putWord8 0xff
    f x = putWord8 x

-- @truncatedInt n v@ returns the last n bytes of v, encoded big endian.
truncatedInt :: Int -> Integer -> ByteString
truncatedInt n v = BS.drop (8 - n) (Put.runPut (Put.putWord64be $ fromIntegral v))

encodeLargePosInt :: Integer -> PutTuple ()
encodeLargePosInt v = do
  let l = byteLen v
  when (l > 255) (throw (Error TupleIntTooLarge))
  putWord8 (fromIntegral l)
  forM_ [l -1, l -2 .. 0] $ \i ->
    putWord8 (fromIntegral (v `shiftR` (8 * i)))

encodeLargeNegInt :: Integer -> PutTuple ()
encodeLargeNegInt v = do
  let l = byteLen v
  when (l > 255) (throw (Error TupleIntTooLarge))
  let v' = v + (1 `shiftL` (8 * l)) - 1 :: Integer
  putWord8 (fromIntegral l `xor` 0xff)
  forM_ [l -1, l -2 .. 0] $ \i ->
    putWord8 (fromIntegral (v' `shiftR` (8 * i)))

encodePosInt :: Integer -> PutTuple ()
encodePosInt v =
  if v > sizeLimits A.! snd (A.bounds sizeLimits)
    then putWord8 posEndCode >> encodeLargePosInt v
    else do
      let n = bisectSize v
      putWord8 (zeroCode + fromIntegral n)
      putByteString $ truncatedInt n v

encodeNegInt :: Integer -> PutTuple ()
encodeNegInt v =
  if (negate v) > sizeLimits A.! snd (A.bounds sizeLimits)
    then putWord8 negStartCode >> encodeLargeNegInt v
    else do
      let n = bisectSize (negate v)
      let maxv = sizeLimits A.! n
      putWord8 (zeroCode - fromIntegral n)
      putByteString $ truncatedInt n (maxv + v)

-- | given an IEEE 754 float/double, adjust it for encoding.
floatAdjust :: Bool -> ByteString -> ByteString
floatAdjust isEncode bs
  | isEncode && (BS.head bs .&. 0x80) /= 0 = BS.map (xor 0xff) bs
  | not isEncode && (BS.head bs .&. 0x80) /= 0x80 = BS.map (xor 0xff) bs
  | otherwise = BS.cons (BS.head bs `xor` 0x80) (BS.tail bs)

encodeElem ::
  -- | Whether we are inside a nested tuple
  Bool ->
  -- | elem to encode
  Elem ->
  PutTuple ()
encodeElem True None =
  putWord8 nullCode >> putWord8 0xff
encodeElem False None =
  putWord8 nullCode
encodeElem _ (Bytes bs) =
  putWord8 bytesCode >> encodeBytes bs
encodeElem _ (Text t) =
  putWord8 stringCode >> encodeBytes (encodeUtf8 t)
encodeElem _ (Int 0) = putWord8 zeroCode
encodeElem _ (Int n) = if n > 0 then encodePosInt n else encodeNegInt n
encodeElem _ (Float x) = do
  putWord8 floatCode
  putByteString $ floatAdjust True $ Put.runPut $ Put.putFloat32be x
encodeElem _ (Double x) = do
  putWord8 doubleCode
  putByteString $ floatAdjust True $ Put.runPut $ Put.putFloat64be x
encodeElem _ (Bool True) = putWord8 trueCode
encodeElem _ (Bool False) = putWord8 falseCode
encodeElem _ (UUID w x y z) = do
  putWord8 uuidCode
  putWord32be w
  putWord32be x
  putWord32be y
  putWord32be z
encodeElem _ (Tuple xs) = do
  putWord8 nestedCode
  mapM_ (encodeElem True) xs
  putWord8 0x00
encodeElem _ (CompleteVS (CompleteVersionstamp tvs uv)) = do
  let (TransactionVersionstamp tv tb) = tvs
  putWord8 versionstampCode
  putWord64be tv
  putWord16be tb
  putWord16be uv
encodeElem _ (IncompleteVS (IncompleteVersionstamp uv)) = do
  putWord8 versionstampCode
  s <- get
  put s {incompleteVersionstampPos = Just $ currLength s}
  putWord64be maxBound
  putWord16be maxBound
  putWord16be uv

-- | Encodes a tuple from a list of tuple elements. Returns the encoded
-- tuple.
--
-- Warning: this function can throw an 'Error' with 'TupleIntTooLarge' if you
-- pass an Int element that requires more than 255 bytes to serialize. Since
-- the smallest such number is 614 decimal digits long, we deemed this situation
-- unlikely enough that it wasn't worth returning a sum type from this function.
--
-- Note: this encodes to the format expected by FoundationDB as input, which
-- is slightly different from the format returned by FoundationDB as output. The
-- difference is that if the encoded bytes include an incomplete version stamp,
-- four bytes are appended to the end to indicate the index of the incomplete
-- version stamp so that FoundationDB can fill in the transaction version and
-- batch order when this function is used in conjunction with
-- 'setVersionstampedKey' and 'setVersionstampedValue':
--
-- @
-- do let k = pack mySubspace [IncompleteVS (IncompleteVersionstamp 123)]
--    atomicOp k (setVersionstampedKey "my_value")
-- @
--
-- Because FoundationDB uses two bytes at the end of the key for this, only
-- one 'IncompleteVS' can be used per key.
--
-- This also means that @(decodeTupleElems . encodeTupleElems)@ gives
-- strange results when an 'IncompleteVS' is present in the input, because the
-- two extra bytes are interpreted as being part of the tuple.
--
-- >>> decodeTupleElems $ encodeTupleElems [IncompleteVS (IncompleteVersionstamp 1)]
-- Right [IncompleteVS (IncompleteVersionstamp 1),Bytes "",None,None]
--
-- For this reason, 'decodeTupleElems' should only be called on keys that have
-- been returned from the database, because 'setVersionstampedKey' drops
-- the last two bytes when it writes the key to the database.
encodeTupleElems :: Traversable t => t Elem -> ByteString
encodeTupleElems = fst . runPutTuple . mapM_ (encodeElem False)

-- | Like 'encodeTupleElems', but prepends a raw bytestring prefix to the
-- tuple. This is used by the subspace and directory layers.
encodeTupleElemsWPrefix :: Traversable t => ByteString -> t Elem -> ByteString
encodeTupleElemsWPrefix prefix es =
  fst $
    runPutTuple $ do
      putByteString prefix
      mapM_ (encodeElem False) es

-- | Decodes a tuple, or returns a parse error. This function will never return
-- 'IncompleteVS' tuple elements. See the note on 'encodeTupleElems' for more
-- information.
decodeTupleElems :: ByteString -> Either String [Elem]
decodeTupleElems bs = runGetComplete bs $ many (decodeElem False)

-- | Decodes a tuple that was encoded with a given prefix. Fails if the
-- input prefix is not actually a prefix of the encoded tuple.
decodeTupleElemsWPrefix ::
  -- | expected prefix
  ByteString ->
  -- | encoded tuple
  ByteString ->
  Either String [Elem]
decodeTupleElemsWPrefix prefix bs = runGetComplete bs $ do
  gotPrefix <- getByteString (BS.length prefix)
  guard (gotPrefix == prefix)
  many (decodeElem False)

runGetComplete :: ByteString -> Get a -> Either String a
runGetComplete bs decoder = do
  (result, rest) <- runGetState decoder bs 0
  unless (BS.null rest) $
    Left $ "could not decode " <> show (BS.length rest) <> " bytes from the end of the bytestring"
  pure result

decodeElem :: Bool -> Get Elem
decodeElem nested =
  getWord8 >>= \case
    c
      | c == nullCode && nested -> expectCode 0xff >> return None
      | c == nullCode -> return None
      | c == bytesCode -> decodeBytesElem
      | c == stringCode -> decodeTextElem
      | c >= zeroCode && c < posEndCode -> decodeSmallPosInt c
      | c > negStartCode && c < zeroCode -> decodeSmallNegInt c
      | c == posEndCode -> decodeLargePosInt
      | c == negStartCode -> decodeLargeNegInt
      | c == floatCode -> decodeFloatElem
      | c == doubleCode -> decodeDoubleElem
      | c == trueCode -> return (Bool True)
      | c == falseCode -> return (Bool False)
      | c == uuidCode -> decodeUUIDElem
      | c == nestedCode -> decodeTupleElem
      | c == versionstampCode -> decodeVersionstamp
    c -> fail $ "got unknown tuple code: " ++ show c

expectCode :: Word8 -> Get ()
expectCode c = do
  c' <- getWord8
  guard (c == c')

bytesTerminator :: Get ()
bytesTerminator =
  do
    n <- remaining
    guard (n == 0)
    <|> do
      c <- getWord8
      n <- remaining
      guard (c == 0x00 && n == 0)
    <|> do
      c <- getWord8
      d <- lookAhead getWord8
      guard (c == 0x00 && d /= 0xff)

-- | Reads all bytes up to (but not including) the terminator byte
getBytesUntilTerminator :: Get ByteString
getBytesUntilTerminator = BS.pack <$> many nonTerminator
  where
    nonTerminator = do
      a <- getWord8
      if a == 0
        then do
          b <- getWord8
          guard (b == 0xff)
          pure 0
        else do
          pure a

decodeBytesElem :: Get Elem
decodeBytesElem =
  Bytes <$> (getBytesUntilTerminator <* bytesTerminator)

decodeTextElem :: Get Elem
decodeTextElem =
  Text . decodeUtf8 <$> (getBytesUntilTerminator <* bytesTerminator)

decodeSmallPosInt :: Word8 -> Get Elem
decodeSmallPosInt code = do
  let n = fromIntegral $ code - 20
  bs <- (BS.pack (replicate (8 - n) 0x00) <>) <$> getBytes n
  let subres = runGet getWord64be bs
  case subres of
    Left e -> fail e
    Right x -> return $ Int $ fromIntegral x

decodeSmallNegInt :: Word8 -> Get Elem
decodeSmallNegInt code = do
  let n = fromIntegral $ 20 - code
  bs <- (BS.pack (replicate (8 - n) 0x00) <>) <$> getBytes n
  let subres = runGet getWord64be bs
  case subres of
    Left e -> fail e
    Right x -> return $ Int $ fromIntegral x - sizeLimits A.! n

decodeLargeNegInt :: Get Elem
decodeLargeNegInt = do
  (n :: Int) <- fromIntegral . xor 0xff <$> getWord8
  val <- go 0 n 0
  return $ Int (val - (1 `shiftL` (n * 8)) + 1)
  where
    go !i !n !x
      | i == n = return x
      | otherwise = do
        d <- fromIntegral <$> getWord8
        go (i + 1) n (d + (x `shiftL` 8))

decodeLargePosInt :: Get Elem
decodeLargePosInt = do
  (n :: Int) <- fromIntegral <$> getWord8
  go 0 n 0
  where
    go !i !n !x
      | i == n = return $ Int x
      | otherwise = do
        d <- fromIntegral <$> getWord8
        go (i + 1) n (d + (x `shiftL` 8))

decodeFloatElem :: Get Elem
decodeFloatElem = do
  fBytes <- floatAdjust False <$> getByteString 4
  let subres = runGet getFloat32be fBytes
  case subres of
    Left e -> fail e
    Right x -> return $ Float x

decodeDoubleElem :: Get Elem
decodeDoubleElem = do
  fBytes <- floatAdjust False <$> getByteString 8
  let subres = runGet getFloat64be fBytes
  case subres of
    Left e -> fail e
    Right x -> return $ Double x

decodeUUIDElem :: Get Elem
decodeUUIDElem =
  UUID <$> getWord32be
    <*> getWord32be
    <*> getWord32be
    <*> getWord32be

decodeTupleElem :: Get Elem
decodeTupleElem = do
  ts <- loop
  terminator <- getWord8
  guard (terminator == 0)
  return (Tuple ts)
  where
    loop = do
      isEnd <- checkEnd
      if isEnd
        then return []
        else do
          next <- decodeElem True
          rest <- loop
          return (next : rest)

    checkEnd :: Get Bool
    checkEnd = lookAhead $ do
      c <- getWord8
      if c == 0x00
        then ((/= 0xff) <$> getWord8) <|> return True
        else return False

decodeVersionstamp :: Get Elem
decodeVersionstamp = do
  tv <- getWord64be
  bo <- getWord16be
  uv <- getWord16be
  let tvs = TransactionVersionstamp tv bo
  return $ CompleteVS $ CompleteVersionstamp tvs uv
