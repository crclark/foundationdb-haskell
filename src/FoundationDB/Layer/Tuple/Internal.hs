{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module FoundationDB.Layer.Tuple.Internal where

import FoundationDB.VersionStamp

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans (lift)
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as A
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Serialize.IEEE754 as Put
import qualified Data.Serialize.Put as Put
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Exts
import GHC.Integer.Logarithms
import qualified GHC.TypeLits as TL
import Numeric.Search.Range (searchFromTo)

-- | Crude UUID type to avoid dependency on UUID library. Interconvertible with
-- @toWords@ and @fromWords@ in 'Data.UUID'.
data UUID = UUID Word32 Word32 Word32 Word32
  deriving (Show, Eq, Ord)

data Elem =
  TNone
  | TBytes ByteString
  | TText T.Text
  | TTuple [Elem]
  | TInt Int
  | TFloat Float
  | TDouble Double
  | TBool Bool
  | TUUID Word32 Word32 Word32 Word32
  | TCVersionStamp (VersionStamp 'Complete)
  | TIVersionStamp (VersionStamp 'Incomplete)

deriving instance Show Elem
deriving instance Ord Elem
deriving instance Eq Elem

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
              putWord8 ((fromIntegral v `shiftR` (fromIntegral $ 8*i)) .&. 0xff)
    else do let n = bisectSize v
            putWord8 (zeroCode + fromIntegral n)
            putByteString $ truncatedInt n v

encodeNegInt :: Int -> PutTuple ()
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
floatAdjust isEncode bs =
  if isEncode && (BS.head bs .&. 0x80) /= 0
    then BS.map (xor 0xff) bs
    else if not isEncode && (BS.head bs .&. 0x80) /= 0x80
      then BS.map (xor 0xff) bs
      else BS.cons (BS.head bs `xor` 0x80) (BS.tail bs)

encodeElem :: Bool
           -- ^ Whether we are inside a nested tuple
           -> Elem
           -- ^ elem to encode
           -> PutTuple ()
encodeElem True TNone =
  putWord8 nullCode >> putWord8 0xff
encodeElem False TNone =
  putWord8 nullCode
encodeElem _ (TBytes bs) =
  putWord8 bytesCode >> encodeBytes bs
encodeElem _ (TText t) =
  putWord8 stringCode >> encodeBytes (encodeUtf8 t)
encodeElem _ (TInt 0) = putWord8 zeroCode
encodeElem _ (TInt n) = if n > 0 then encodePosInt n else encodeNegInt n
encodeElem _ (TFloat x) = do
  putWord8 floatCode
  putByteString $ floatAdjust True $ Put.runPut $ Put.putFloat32be x
encodeElem _ (TDouble x) = do
  putWord8 doubleCode
  putByteString $ floatAdjust True $ Put.runPut $ Put.putFloat64be x
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
encodeElem _ (TCVersionStamp (CompleteVersionStamp tv tb uv)) = do
  putWord8 versionstampCode
  putWord64be tv
  putWord16be tb
  putWord16be uv
encodeElem _ (TIVersionStamp (IncompleteVersionStamp uv)) = do
  putWord8 versionstampCode
  s <- get
  put s{incompleteVersionStampPos = Just $ currLength s}
  putWord64be 0xffffffffffffffff
  putWord16be 0xffff
  putWord16be uv

encodeTupleElems :: [Elem] -> (ByteString, Maybe Int)
encodeTupleElems = runPutTuple . mapM_ (encodeElem False)

encodeTupleElems' :: [Elem] -> ByteString
encodeTupleElems' = fst . runPutTuple . mapM_ (encodeElem False)

data ContainsIncompleteness = NoIncomplete | WithIncomplete

class Tuplable a where
  encodeTupleElem :: Bool -> a -> PutTuple ()

instance Tuplable () where
  encodeTupleElem b _ = encodeElem b TNone

instance Tuplable ByteString where
  encodeTupleElem b bs = encodeElem b (TBytes bs)

instance Tuplable T.Text where
  encodeTupleElem b t = encodeElem b (TText t)

instance Tuplable Int where
  encodeTupleElem b x = encodeElem b (TInt x)

instance Tuplable Float where
  encodeTupleElem b x = encodeElem b (TFloat x)

instance Tuplable Double where
  encodeTupleElem b x = encodeElem b (TDouble x)

instance Tuplable Bool where
  encodeTupleElem b x = encodeElem b (TBool x)

instance Tuplable UUID where
  encodeTupleElem b (UUID w1 w2 w3 w4) = encodeElem b (TUUID w1 w2 w3 w4)

instance Tuplable (VersionStamp a) where
  encodeTupleElem b x@(CompleteVersionStamp _ _ _) =
    encodeElem b (TCVersionStamp x)
  encodeTupleElem b x@(IncompleteVersionStamp _) =
    encodeElem b (TIVersionStamp x)

type family UpdateContainsIncomplete (ci :: ContainsIncompleteness) b where
  UpdateContainsIncomplete 'NoIncomplete (VersionStamp 'Incomplete) =
    'WithIncomplete
  UpdateContainsIncomplete 'NoIncomplete _ =
    'NoIncomplete
  UpdateContainsIncomplete 'WithIncomplete (VersionStamp 'Incomplete) =
    TL.TypeError
    ('TL.Text "Tuples may contain at most one incomplete VersionStamp.")
  UpdateContainsIncomplete 'WithIncomplete _ =
    'WithIncomplete

infixr 5 :/:

data Tuple (a :: ContainsIncompleteness) where
  Nil :: Tuple 'NoIncomplete
  (:/:) :: (Tuplable a)
           => a -> Tuple ci -> Tuple (UpdateContainsIncomplete ci a)

encodeTuple :: Tuple a -> ByteString
encodeTuple Nil = mempty
encodeTuple t = fst $ runPutTuple $ go t
  where go :: Tuple a -> PutTuple ()
        go Nil = return ()
        go (e :/: es) = encodeTupleElem False e >> go es
