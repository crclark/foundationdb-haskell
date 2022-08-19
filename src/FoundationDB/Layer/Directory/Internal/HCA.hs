{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FoundationDB.Layer.Directory.Internal.HCA where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch (bracket_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize.Get (getWord64le, runGet)
import FoundationDB
import FoundationDB.Layer.Directory.Internal.Error
import FoundationDB.Layer.Subspace
import FoundationDB.Layer.Tuple
import FoundationDB.Options.MutationType (add)
import FoundationDB.Options.TransactionOption (nextWriteNoWriteConflictRange)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (getStdRandom, randomR)

oneBytes :: ByteString
oneBytes = BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

data HCA = HCA
  { counters :: Subspace,
    recent :: Subspace
  }
  deriving (Show, Eq, Ord)

newHCA :: Subspace -> HCA
newHCA s =
  HCA
    { counters = extend s [Int 0],
      recent = extend s [Int 1]
    }

windowSize :: Int -> Int
windowSize start
  | start < 255 = 64
  | start < 65535 = 1024
  | otherwise = 8192

-- | global lock used for allocating. We use this simply because the other
-- clients have it. It appears to exist in order to reduce contention on the HCA
-- counter at the transaction level by pushing some of the contention to the
-- client level instead.
globalAllocateLock :: MVar ()
globalAllocateLock = unsafePerformIO (newMVar ())
{-# NOINLINE globalAllocateLock #-}

withAllocLock :: Transaction a -> Transaction a
withAllocLock =
  bracket_
    (liftIO $ takeMVar globalAllocateLock)
    (liftIO $ putMVar globalAllocateLock ())

findStartAndWindow :: HCA -> Bool -> Int -> Transaction (Int, Int)
findStartAndWindow hca@HCA {..} windowAdvanced start = do
  countFuture <- withAllocLock getCount
  countStr <- await countFuture
  count <- parseCount countStr
  let window = windowSize start
  if count * 2 < window
    then return (start, window)
    else findStartAndWindow hca True (start + window)
  where
    getCount = do
      let start' = fromIntegral start
      when windowAdvanced $ do
        clearRange (pack counters []) (pack counters [Int start'])
        setOption nextWriteNoWriteConflictRange
        clearRange (pack recent []) (pack recent [Int start'])
      atomicOp (pack counters [Int start']) (add oneBytes)
      withSnapshot $ get (pack counters [Int start'])

    parseCount Nothing = return 0
    parseCount (Just bs) =
      case runGet getWord64le bs of
        Left _ -> throwDirInternalError $ "failed to parse count: " ++ show bs
        Right n -> return $ fromIntegral n

findSubspaceLoop ::
  HCA ->
  Subspace ->
  Int ->
  Int ->
  Transaction (Maybe Subspace)
findSubspaceLoop hca@HCA {..} s start window = do
  candidate <- liftIO $ getStdRandom (randomR (start, start + window))
  let key = pack recent [Int $ fromIntegral candidate]
  (latestCounter, candidateValueF) <- withAllocLock $ do
    latestCounter <- withSnapshot $ getLast counters
    candidateValueF <- get key
    setOption nextWriteNoWriteConflictRange
    set key ""
    return (latestCounter, candidateValueF)
  currentStart <- case latestCounter of
    Just (k, _) -> case unpack counters k of
      Right (Int x : _) -> return x
      _ -> throwDirInternalError $ "bad counter format: " ++ show k
    _ -> throwDirInternalError "failed to find latestCounter"
  if currentStart > fromIntegral start
    then return Nothing
    else
      await candidateValueF >>= \case
        Just _ -> findSubspaceLoop hca s start window
        Nothing -> do
          addConflictRange key (key <> "0x00") ConflictRangeTypeWrite
          return $ Just $ extend s [Int $ fromIntegral candidate]

initStart :: HCA -> Transaction Int
initStart HCA {..} = do
  mkv <- withSnapshot $ getLast counters
  case mkv of
    Just (k, _) -> case unpack counters k of
      Right (Int start : _) -> return $ fromIntegral start
      _ -> return 0
    Nothing -> return 0

allocate :: HCA -> Subspace -> Transaction Subspace
allocate hca s = do
  start <- initStart hca
  (start', window) <- findStartAndWindow hca False start
  msub <- findSubspaceLoop hca s start' window
  case msub of
    Just sub -> return sub
    Nothing -> allocate hca s
