{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FoundationDB.Layer.Directory.Internal.HCA where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch (bracket_)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid hiding (getLast)
import Data.Serialize.Get (runGet, getWord64le)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (getStdRandom, randomR)

import FoundationDB
import FoundationDB.Error
import FoundationDB.Layer.Tuple
import FoundationDB.Layer.Subspace
import FoundationDB.Options

throwDirError :: String -> Transaction a
throwDirError = throwError . Error . DirectoryLayerError

oneBytes :: ByteString
oneBytes = BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

data HCA = HCA {counters :: Subspace,
                recent :: Subspace}
  deriving (Show, Eq, Ord)

newHCA :: Subspace -> HCA
newHCA s = HCA
  { counters = s <> subspace [IntElem 0]
  ,  recent  = s <> subspace [IntElem 1]
  }

windowSize :: Int -> Int
windowSize start
  | start < 255   = 64
  | start < 65535 = 1024
  | otherwise     = 8192

-- | global lock used for allocating. We use this simply because the other
-- clients have it. It's not yet clear if it's actually needed. TODO: find out.
globalAllocateLock :: MVar ()
globalAllocateLock = unsafePerformIO (newMVar ())
{-# NOINLINE globalAllocateLock #-}

withAllocLock :: Transaction a -> Transaction a
withAllocLock =
  bracket_ (liftIO $ takeMVar globalAllocateLock)
           (liftIO $ putMVar globalAllocateLock ())

findStartAndWindow :: HCA -> Bool -> Int -> Transaction (Int,Int)
findStartAndWindow hca@HCA{..} windowAdvanced start = do
  countFuture <- withAllocLock getCount
  countStr <- await countFuture
  count <- parseCount countStr
  let window = windowSize start
  if count*2 < window
    then return (start, window)
    else findStartAndWindow hca True (start + window)

  where getCount = do
          when windowAdvanced $ do
            clearRange (pack counters []) (pack counters [IntElem start])
            setOption nextWriteNoWriteConflictRange
            clearRange (pack recent []) (pack recent [IntElem start])
          atomicOp Add (pack counters [IntElem start]) oneBytes
          withSnapshot $ get (pack counters [IntElem start])

        parseCount Nothing = return 0
        parseCount (Just bs) =
          case runGet getWord64le bs of
            Left _ -> throwDirError "failed to parse count"
            Right n -> return $ fromIntegral n

findSubspaceLoop :: HCA
                 -> Subspace
                 -> Int
                 -> Int
                 -> Transaction (Maybe Subspace)
findSubspaceLoop hca@HCA{..} s start window = do
  candidate <- liftIO $ getStdRandom (randomR (start, start + window))
  let key = pack recent [IntElem candidate]
  (latestCounter, candidateValueF) <- withAllocLock $ do
    latestCounter <- withSnapshot $ getLast counters
    candidateValueF <- get key
    setOption nextWriteNoWriteConflictRange
    set key ""
    return (latestCounter, candidateValueF)
  currentStart <- case latestCounter of
                    Just (k,_) -> case unpack counters k of
                                    Right (IntElem x:_) -> return x
                                    _ -> throwDirError "bad counter format"
                    _ -> throwDirError "failed to find latestCounter"
  if currentStart > start
    then return Nothing
    else await candidateValueF >>= \case
      Just _ -> findSubspaceLoop hca s start window
      Nothing -> do
        addConflictRange key (key <> "0x00") ConflictRangeTypeWrite
        return $ Just $ s <> subspace [IntElem candidate]

allocate :: HCA -> Subspace -> Transaction Subspace
allocate hca@HCA{..} s = do
  mkv <- withSnapshot $ getLast counters
  case mkv of
    Just (k,_) -> do
      let Right [IntElem start] = unpack counters k
      (start', window) <- findStartAndWindow hca False start
      msub <- findSubspaceLoop hca s start' window
      case msub of
        Just sub -> return sub
        Nothing -> allocate hca s
    _ -> throwDirError "failed to get last counter"
