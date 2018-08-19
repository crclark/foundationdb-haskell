{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module FoundationDB.Transaction (
  -- * Transactions
    Transaction
  , runTransaction
  , runTransaction'
  , TransactionConfig(..)
  , runTransactionWithConfig
  , runTransactionWithConfig'
  , cancel
  , withSnapshot
  , setOption
  , get
  , set
  , clear
  , clearRange
  , addConflictRange
  , FDB.FDBConflictRangeType
  , getKey
  , getKeyAddresses
  , atomicOp
  , getRange
  , getRange'
  , FDB.FDBStreamingMode(..)
  , getEntireRange
  , isRangeEmpty
  , Range(..)
  , rangeKeys
  , prefixRange
  , RangeResult(..)
  -- * Futures
  , Future
  , await
  -- * Key selectors
  , FDB.KeySelector( LastLessThan
                   , LastLessOrEq
                   , FirstGreaterThan
                   , FirstGreaterOrEq)
  , offset
  -- * Atomic operations
  , AtomicOp(..)
) where

import Control.Exception
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

import FoundationDB.Error
import qualified FoundationDB.Internal.Bindings as FDB
import qualified FoundationDB.Options as FDB

data TransactionEnv = TransactionEnv {
  cTransaction :: FDB.Transaction
  , conf :: TransactionConfig
  } deriving (Show)

createTransactionEnv :: FDB.Database
                       -> TransactionConfig
                       -> ExceptT Error (ResourceT IO) TransactionEnv
createTransactionEnv db conf = do
  (_rk, eTrans) <- allocate (fdbEither $ FDB.databaseCreateTransaction db)
                            (either (const $ return ()) FDB.transactionDestroy)
  liftEither $ fmap (flip TransactionEnv conf) eTrans

-- TODO: this will be exported to users with a MonadIO instance. At first
-- glance, that seems bad, since runTransaction does auto
-- retries. I see a few options in various DB libraries on Hackage:
-- 1. don't allow IO in transactions at all.
-- 2. don't even create a separate transaction monad; use IO for everything.
-- 3. Export a TransactionT with MonadIO m => MonadIO (TransactionT m)
--    so that users can decide whether they want to deal with the risk.
-- I'm leaning towards 3. We can export both Transaction and TransactionT.
newtype Transaction a = Transaction
  {unTransaction :: ReaderT TransactionEnv (ExceptT Error (ResourceT IO)) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadCatch,
            MonadMask)
-- TODO: ok to have both MonadThrow and MonadError instances?
deriving instance MonadError Error Transaction
deriving instance MonadReader TransactionEnv Transaction
deriving instance MonadResource Transaction

data Future a = forall b. Future
  { _cFuture :: FDB.Future b
  , _extractValue :: Transaction a
  }

instance Show (Future a) where
  show (Future c _) = show $ "Future " ++ show c

fromCExtractor :: FDB.Future b
               -> ReleaseKey
               -> Transaction a
               -> Transaction (Future a)
fromCExtractor cFuture rk m =
  return $ Future cFuture $ do
    futErr <- liftIO $ FDB.futureGetError cFuture
    if FDB.isError futErr
      then release rk >> throwError (CError $ toError futErr)
      else do
        res <- m
        release rk
        return res

allocFuture :: IO (FDB.Future b)
            -> (FDB.Future b -> Transaction a)
            -> Transaction (Future a)
allocFuture make extract = do
  (rk, future) <- allocate make FDB.futureDestroy
  fromCExtractor future rk (extract future)

-- | Block until a future is ready.
await :: Future a -> Transaction a
await (Future f e) = do
  fdbExcept' $ FDB.futureBlockUntilReady f
  e

commitFuture :: Transaction (Future ())
commitFuture = do
  t <- asks cTransaction
  allocFuture (FDB.transactionCommit t) (const $ return ())


-- | Get the value of a key. If the key does not exist, returns 'Nothing'.
get :: ByteString -> Transaction (Future (Maybe ByteString))
get key = do
  t <- ask
  let isSnapshot = snapshotReads (conf t)
  allocFuture (FDB.transactionGet (cTransaction t) key isSnapshot)
              (\f -> liftIO (FDB.futureGetValue f) >>= liftFDBError)

-- | Set a bytestring key to a bytestring value.
set :: ByteString -> ByteString -> Transaction ()
set key val = do
  t <- asks cTransaction
  liftIO $ FDB.transactionSet t key val

-- | Delete a key from the DB.
clear :: ByteString -> Transaction ()
clear k = do
  t <- asks cTransaction
  liftIO $ FDB.transactionClear t k

-- | @clearRange k l@ deletes all keys in the half-open range [k,l).
clearRange :: ByteString -> ByteString -> Transaction ()
clearRange k l = do
  t <- asks cTransaction
  liftIO $ FDB.transactionClearRange t k l

addConflictRange :: ByteString
                 -> ByteString
                 -> FDB.FDBConflictRangeType
                 -> Transaction ()
addConflictRange k l ty = do
  t <- asks cTransaction
  liftIO $ fdbThrowing $ FDB.transactionAddConflictRange t k l ty


offset :: FDB.KeySelector -> Int -> FDB.KeySelector
offset (FDB.WithOffset n ks) m = FDB.WithOffset (n+m) ks
offset ks n = FDB.WithOffset n ks

getKey :: FDB.KeySelector -> Transaction (Future ByteString)
getKey ks = do
  t <- asks cTransaction
  isSnapshot <- asks (snapshotReads . conf)
  let (k, orEqual, offsetN) = FDB.keySelectorTuple ks
  allocFuture (FDB.transactionGetKey t k orEqual offsetN isSnapshot)
              (\f -> liftIO (FDB.futureGetKey f) >>= liftFDBError)

getKeyAddresses :: ByteString -> Transaction (Future [ByteString])
getKeyAddresses k = do
  t <- asks cTransaction
  allocFuture (FDB.transactionGetAddressesForKey t k)
              (\f -> liftIO (FDB.futureGetStringArray f) >>= liftFDBError)

-- | Specifies a range of keys to be iterated over by 'getRange'.
data Range = Range {
  rangeBegin :: FDB.KeySelector
  -- ^ The beginning of the range, including this key.
  , rangeEnd :: FDB.KeySelector
  -- ^ The end of the range, not including this key.
  , rangeLimit :: Maybe Int
  -- ^ If the range contains more than @n@ items, return only @Just n@.
  -- If @Nothing@ is provided, returns the entire range.
  , rangeReverse :: Bool
  -- ^ If 'True', return the range in reverse order.
} deriving (Show, Eq, Ord)

-- | @prefixRange prefix@ is the range of all keys of which @prefix@ is a
--   prefix. Returns @Nothing@ if @prefix@ is empty or contains only @0xff@.
prefixRange :: ByteString -> Maybe Range
prefixRange prefix
  | prefix == BS.empty = Nothing
  | BS.all (== 0xff) prefix = Nothing
  | otherwise = Just $ Range
  { rangeBegin = FDB.FirstGreaterOrEq prefix
  , rangeEnd = FDB.FirstGreaterOrEq end
  , rangeLimit = Nothing
  , rangeReverse = False
  }
  where end = let prefix' = BS.takeWhile (/= 0xff) prefix
                  in BS.snoc (BS.init prefix')
                             (BS.last prefix' + 1)

rangeKeys :: Range -> (ByteString, ByteString)
rangeKeys (Range b e _ _) = (FDB.keySelectorBytes b, FDB.keySelectorBytes e)

-- | Structure for returning the result of 'getRange' in chunks.
data RangeResult =
  RangeDone [(ByteString, ByteString)]
  | RangeMore [(ByteString, ByteString)] (Future RangeResult)
  deriving Show

getRange' :: Range -> FDB.FDBStreamingMode -> Transaction (Future RangeResult)
getRange' Range {..} mode = do
  t          <- asks cTransaction
  isSnapshot <- asks (snapshotReads . conf)
  let (beginK, beginOrEqual, beginOffset) = FDB.keySelectorTuple rangeBegin
  let (endK, endOrEqual, endOffset)       = FDB.keySelectorTuple rangeEnd
  let mk = FDB.transactionGetRange t
                                   beginK
                                   beginOrEqual
                                   beginOffset
                                   endK
                                   endOrEqual
                                   endOffset
                                   (fromMaybe 0 rangeLimit)
                                   0
                                   mode
                                   1
                                   isSnapshot
                                   rangeReverse
  let
    handler bsel esel i lim fut = do
      --TODO: need to return Vector or Array for efficiency
      (kvs, more) <- liftIO (FDB.futureGetKeyValueArray fut) >>= liftFDBError
      -- more doesn't take into account our count limit
      let actuallyMore = case lim of
            Nothing -> not (null kvs) && more
            Just n  -> not (null kvs) && length kvs < n && more
      if actuallyMore
        then do
          -- last is partial, but access guarded by @more@
          let lstK = snd $ last kvs
          let bsel' =
                if not rangeReverse then FDB.FirstGreaterThan lstK else bsel
          let (beginK', beginOrEqual', beginOffset') =
                FDB.keySelectorTuple bsel'
          let esel' = if rangeReverse then FDB.FirstGreaterOrEq lstK else esel
          let (endK', endOrEqual', endOffset') = FDB.keySelectorTuple esel'
          let lim' = fmap (\x -> x - length kvs) lim
          let mk' = FDB.transactionGetRange t
                                            beginK'
                                            beginOrEqual'
                                            beginOffset'
                                            endK'
                                            endOrEqual'
                                            endOffset'
                                            (fromMaybe 0 lim')
                                            0
                                            mode
                                            (i + 1)
                                            isSnapshot
                                            rangeReverse
          res <- allocFuture mk' (handler bsel' esel' (i + 1) lim')
          return $ RangeMore kvs res
        else return $ RangeDone $ case lim of
          Nothing -> kvs
          Just n  -> take n kvs
  allocFuture mk (handler rangeBegin rangeEnd 1 rangeLimit)


getRange :: Range -> Transaction (Future RangeResult)
getRange r = getRange' r FDB.StreamingModeIterator

-- TODO: slow and leaky! no lists!
getEntireRange :: Range
               -> Transaction [(ByteString, ByteString)]
getEntireRange r = do
  rr <- getRange' r FDB.StreamingModeWantAll >>= await
  go rr

  where go (RangeDone xs) = return xs
        go (RangeMore xs fut) = do
          more <- await fut
          ys <- go more
          return (xs ++ ys)

isRangeEmpty :: Range -> Transaction Bool
isRangeEmpty r = do
  rr <- getRange r >>= await
  case rr of
    RangeDone [] -> return True
    _            -> return False

data AtomicOp =
  Add
  | And
  | BitAnd
  | Or
  | BitOr
  | Xor
  | BitXor
  | Max
  | Min
  | SetVersionStampedKey
  | SetVersionStampedValue
  | ByteMin
  | ByteMax
  deriving (Enum, Eq, Ord, Show, Read)

toFDBMutationType :: AtomicOp -> FDB.FDBMutationType
toFDBMutationType Add = FDB.MutationTypeAdd
toFDBMutationType And = FDB.MutationTypeAnd
toFDBMutationType BitAnd = FDB.MutationTypeBitAnd
toFDBMutationType Or = FDB.MutationTypeOr
toFDBMutationType BitOr = FDB.MutationTypeBitOr
toFDBMutationType Xor = FDB.MutationTypeXor
toFDBMutationType BitXor = FDB.MutationTypeBitXor
toFDBMutationType Max = FDB.MutationTypeMax
toFDBMutationType Min = FDB.MutationTypeMin
toFDBMutationType SetVersionStampedKey = FDB.MutationTypeSetVersionstampedKey
toFDBMutationType SetVersionStampedValue =
  FDB.MutationTypeSetVersionstampedValue
toFDBMutationType ByteMin = FDB.MutationTypeByteMin
toFDBMutationType ByteMax = FDB.MutationTypeByteMax

atomicOp :: AtomicOp -> ByteString -> ByteString -> Transaction ()
atomicOp op k x = do
  t <- asks cTransaction
  liftIO $ FDB.transactionAtomicOp t k x (toFDBMutationType op)

-- | Contains useful options that are not directly exposed by the C API (for
--   options that are, see 'setOption').
data TransactionConfig = TransactionConfig {
  idempotent :: Bool
  -- ^ When set to 'True' (default is 'False'), running the transaction will
  -- retry even on errors where the transaction may have completed successfully.
  -- When 'False', the transaction will retry only when it is guaranteed that
  -- the transaction was not committed.
  , snapshotReads :: Bool
  -- ^ When set to 'True' (default is 'False'), reads will see the effects of
  -- concurrent transactions, removing the default serializable isolation
  -- guarantee. To enable this feature selectively within a transaction,
  -- see 'withSnapshot'.
  } deriving (Show, Read, Eq, Ord)


defaultConfig :: TransactionConfig
defaultConfig = TransactionConfig False False

runTransaction :: FDB.Database -> Transaction a -> IO a
runTransaction = runTransactionWithConfig defaultConfig

runTransaction' :: FDB.Database -> Transaction a -> IO (Either Error a)
runTransaction' = runTransactionWithConfig' defaultConfig

-- | Attempt to commit a transaction against the given database. If an
-- unretryable error occurs, throws an 'Error'. Attempts to retry the
-- transaction for retryable errors.
runTransactionWithConfig :: TransactionConfig
                         -> FDB.Database
                         -> Transaction a
                         -> IO a
runTransactionWithConfig conf db t = do
  res <- runTransactionWithConfig' conf db t
  case res of
    Left err -> throwIO err
    Right x -> return x

-- TODO: the docs say "on receiving an error from fdb_transaction_*", but we're
-- calling this upon receiving an error from any fdb function. Will that cause
-- problems?
-- | Handles the retry logic described in the FDB docs.
-- https://apple.github.io/foundationdb/api-c.html#c.fdb_transaction_on_error
withRetry :: Transaction a -> Transaction a
withRetry t = catchError t $ \err -> do
  idem <- asks (idempotent . conf)
  let shouldRetry = if idem then retryable else retryableNotCommitted
  if shouldRetry err
    then do
      -- retryable and retryableNotCommitted can only return true for CErrors.
      let (CError err') = err
      trans <- asks cTransaction
      f <- allocFuture (FDB.transactionOnError trans (toCFDBError err'))
                       (const $ return ())
      await f
      -- await throws any error on the future, so if we reach here, we can retry
      withRetry t
    else throwError err

-- Attempt to commit a transaction against the given database. If an unretryable
-- error occurs, returns 'Left'. Attempts to retry the transaction for retryable
-- errors.
runTransactionWithConfig' :: TransactionConfig
                          -> FDB.Database
                          -> Transaction a
                          -> IO (Either Error a)
runTransactionWithConfig' conf db t =
  runResourceT $ runExceptT $ do
    trans <- createTransactionEnv db conf
    flip runReaderT trans $ unTransaction $ withRetry $ do
      res <- t
      commit <- commitFuture
      await commit
      return res

-- | Cancel a transaction. The transaction will not be committed, and
-- will throw 'TransactionCanceled'.
cancel :: Transaction ()
cancel = do
  t <- asks cTransaction
  liftIO $ FDB.transactionCancel t
  throwError (CError TransactionCanceled)

withSnapshot :: Transaction a -> Transaction a
withSnapshot = local $ \s ->
  TransactionEnv (cTransaction s) ((conf s) {snapshotReads = True})

-- | Set one of the transaction options from the underlying C API.
setOption :: FDB.TransactionOption -> Transaction ()
setOption opt = do
  t <- asks cTransaction
  fdbExcept' $ FDB.transactionSetOption t opt
