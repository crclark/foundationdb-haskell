{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module FoundationDB.Transaction (
  -- * Transactions
    Transaction
  , runTransaction
  , runTransaction'
  , TransactionConfig(..)
  , defaultConfig
  , runTransactionWithConfig
  , runTransactionWithConfig'
  , cancel
  , reset
  , withSnapshot
  , setOption
  , getReadVersion
  , setReadVersion
  , getVersionstamp
  , get
  , set
  , clear
  , clearRange
  , addConflictRange
  , FDB.FDBConflictRangeType(..)
  , addReadConflictKey
  , addWriteConflictKey
  , getKey
  , getKeyAddresses
  , atomicOp
  , getRange
  , getRange'
  , FDB.FDBStreamingMode(..)
  , getEntireRange
  , getEntireRange'
  , isRangeEmpty
  , Range(..)
  , rangeKeys
  , keyRange
  , keyRangeInclusive
  , prefixRange
  , prefixRangeEnd
  , RangeResult(..)
  , watch
  -- * Futures
  , Future
  , await
  , awaitInterruptible
  , cancelFuture
  , futureIsReady
  , FutureIO
  , awaitIO
  , awaitInterruptibleIO
  , cancelFutureIO
  , futureIsReadyIO
  -- * Key selectors
  , FDB.KeySelector( LastLessThan
                   , LastLessOrEq
                   , FirstGreaterThan
                   , FirstGreaterOrEq)
  , offset
  -- * Advanced Usage
  -- $advanced
  , TransactionEnv (envConf)
  , createTransactionEnv
  , onEnv
  , commitFuture
  , onError
  , getCommittedVersion
) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ask, asks, local, MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Resource ( allocate
                                    , MonadResource
                                    , release
                                    , ReleaseKey
                                    , ResourceT
                                    , runResourceT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, fromJust)
import Data.Semigroup ((<>))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(Empty, (:|>)))
import Data.Word (Word64)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (castPtr)

import FoundationDB.Error.Internal
import qualified FoundationDB.Internal.Bindings as FDB
import qualified FoundationDB.Options as Opt
import FoundationDB.Versionstamp

-- | A transaction monad. This is currently exported with a 'MonadIO' instance,
-- but using it comes with caveats:
--
--   - 'runTransaction' will retry your transaction in some cases, which means
--     any IO in your transaction will be repeated.
--
--   - Transactions have strict time limits, so slow IO operations should be
--     avoided.
newtype Transaction a = Transaction
  {unTransaction :: ReaderT TransactionEnv (ExceptT Error (ResourceT IO)) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadCatch,
            MonadMask)
-- TODO: ok to have both MonadThrow and MonadError instances?
deriving instance MonadError Error Transaction
-- TODO: does this MonadReader instance need to be exposed?
deriving instance MonadReader TransactionEnv Transaction
deriving instance MonadResource Transaction

-- | A future result of a FoundationDB call. You can block on a future with
-- 'await'.
-- WARNING: returning a value of this type from 'runTransaction' and then
-- calling 'await' on the value in another transaction will cause a segfault!
-- Future versions of this library may use more sophisticated types to prevent
-- this.
data Future a =
  PureFuture a
  -- ^ For applicative and monad instances
  | forall b. Future
  -- TODO:the future is closed over by _extractValue. It's only included in this
  -- record so that we can more easily debug (see the Show instance, which
  -- prints the address). Given the lack of bugs so far, it is probably safe
  -- to simplify this.
  { _cFuture :: FDB.Future b
  , _extractValue :: Transaction a
  }

instance Show a => Show (Future a) where
  show (PureFuture x) = show $ "Future " ++ show x
  show (Future c _) = show $ "Future " ++ show c

instance Functor Future where
  fmap f (PureFuture x) = PureFuture (f x)
  fmap f (Future cf e) = Future cf (fmap f e)

instance Applicative Future where
  pure = PureFuture
  (PureFuture f)   <*> (PureFuture x)   = PureFuture (f x)
  (PureFuture f)   <*> fut@(Future _ _) = fmap f fut
  fut@(Future _ _) <*> (PureFuture x)   = fmap ($ x) fut
  fut@(Future _ _) <*> (Future cf e)    = Future cf (await fut <*> e)

fromCExtractor
  :: FDB.Future b -> ReleaseKey -> Transaction a -> Transaction (Future a)
fromCExtractor cFuture rk extract = return $ Future cFuture $ do
  futErr <- liftIO $ FDB.futureGetError cFuture
  case toError futErr of
    Just x  -> release rk >> throwError (CError x)
    Nothing -> do
      res <- extract
      release rk
      return res

allocFuture
  :: IO (FDB.Future b)
  -> (FDB.Future b -> Transaction a)
  -> Transaction (Future a)
allocFuture make extract = do
  (rk, future) <- allocate make FDB.futureDestroy
  fromCExtractor future rk (extract future)

-- | Block until a future is ready. Unfortunately, does not seem to be
-- interruptible SIGPIPE (the interrupt sent by Control.Conccurent.Async to
-- cancel), even when using InterruptibleFFI.
await :: Future a -> Transaction a
await (PureFuture x) = return x
await (Future f e) = do
  fdbExcept' $ FDB.futureBlockUntilReady f
  e

-- | Polls a future for readiness in a loop until it is ready, then returns
-- the value in the future. This is less resource efficient than 'await', but
-- can be interrupted more easily.
awaitInterruptible :: Future a -> Transaction a
awaitInterruptible (PureFuture x) = return x
awaitInterruptible fut@(Future _f e) = futureIsReady fut >>= \case
  True -> e
  False -> liftIO (threadDelay 1000) >> awaitInterruptible fut

-- | Cancel a future. Attempts to await the future after cancellation will throw
-- 'OperationCancelled'.
cancelFuture :: Future a -> Transaction ()
cancelFuture (PureFuture _) = return ()
cancelFuture (Future f _e) = liftIO $ FDB.futureCancel f

-- | Returns True if the future is ready. If so, calling 'await' will not block.
futureIsReady :: Future a -> Transaction Bool
futureIsReady (PureFuture _) = return True
futureIsReady (Future f _) = liftIO $ FDB.futureIsReady f

-- | A future that can only be awaited after its transaction has committed.
-- That is, in contrast to 'Future', this __must__ be returned from
-- 'runTransaction' before it can safely be awaited. Use 'awaitIO' to await it.
-- This future type is not needed frequently.
--
-- All 'FutureIO' functions work similarly to their 'Future' counterparts.
data FutureIO a = FutureIO
  { _fgnPtr :: ForeignPtr ()
  , _extractValueIO :: IO a}

instance Show (FutureIO a) where
  show (FutureIO p _) = show $ "FutureIO " ++ show p

instance Functor FutureIO where
  fmap f (FutureIO cf e) = FutureIO cf (fmap f e)

allocFutureIO :: FDB.Future b -> IO a -> IO (FutureIO a)
allocFutureIO (FDB.Future f) e = do
  fp <- newForeignPtr FDB.futureDestroyPtr (castPtr f)
  return $ FutureIO fp e

-- | IO analogue to 'await'.
awaitIO :: FutureIO a -> IO (Either Error a)
awaitIO (FutureIO fp e) = withForeignPtr fp $ \f ->
  fdbEither' (FDB.futureBlockUntilReady (FDB.Future (castPtr f))) >>= \case
    Left  err -> return $ Left err
    Right ()  -> Right <$> e

-- | IO analogue to 'awaitInterruptible'.
awaitInterruptibleIO :: FutureIO a -> IO a
awaitInterruptibleIO fut@(FutureIO _ e) = futureIsReadyIO fut >>= \case
  True -> e
  False -> threadDelay 1000 >> awaitInterruptibleIO fut

-- | Cancel a future. Attempts to await the future after cancellation will throw
-- 'OperationCancelled'.
cancelFutureIO :: FutureIO a -> IO ()
cancelFutureIO (FutureIO fp _e) = withForeignPtr fp $ \f ->
  FDB.futureCancel (FDB.Future (castPtr f))

futureIsReadyIO :: FutureIO a -> IO Bool
futureIsReadyIO (FutureIO fp _) = withForeignPtr fp $ \f ->
  FDB.futureIsReady (FDB.Future (castPtr f))

-- | Attempts to commit a transaction. If 'await'ing the returned 'Future'
-- works without errors, the transaction was committed.
commitFuture :: Transaction (Future ())
commitFuture = do
  t <- asks cTransaction
  allocFuture (FDB.transactionCommit t) (const $ return ())

-- | Get the value of a key. If the key does not exist, returns 'Nothing'.
get :: ByteString -> Transaction (Future (Maybe ByteString))
get key = do
  t <- ask
  let isSnapshot = snapshotReads (envConf t)
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

-- | Tells FoundationDB to consider the given range to have been read by this
-- transaction.
addConflictRange
  :: ByteString -> ByteString -> FDB.FDBConflictRangeType -> Transaction ()
addConflictRange k l ty = do
  t <- asks cTransaction
  fdbExcept' $ FDB.transactionAddConflictRange t k l ty

-- | Tells FoundationDB to consider the given key to have been read by this
-- transaction.
addReadConflictKey :: ByteString -> Transaction ()
addReadConflictKey k =
  addConflictRange k (BS.snoc k 0x00) FDB.ConflictRangeTypeRead

-- | Tells FoundationDB to consider the given key to have been written
-- by this transaction.
addWriteConflictKey :: ByteString -> Transaction ()
addWriteConflictKey k =
  addConflictRange k (BS.snoc k 0x00) FDB.ConflictRangeTypeWrite

-- | Increase the offset of the given 'KeySelector'.
offset :: Int -> FDB.KeySelector -> FDB.KeySelector
offset m (FDB.WithOffset n ks) = FDB.WithOffset (n + m) ks
offset n ks                    = FDB.WithOffset n ks

-- | Gets the key specified by the given 'KeySelector'.
getKey :: FDB.KeySelector -> Transaction (Future ByteString)
getKey ks = do
  t          <- asks cTransaction
  isSnapshot <- asks (snapshotReads . envConf)
  let (k, orEqual, offsetN) = FDB.keySelectorTuple ks
  allocFuture (FDB.transactionGetKey t k orEqual offsetN isSnapshot)
              (\f -> liftIO (FDB.futureGetKey f) >>= liftFDBError)

-- | Get the public network addresses of all nodes responsible for storing
-- the given key.
getKeyAddresses :: ByteString -> Transaction (Future [ByteString])
getKeyAddresses k = do
  t <- asks cTransaction
  allocFuture (FDB.transactionGetAddressesForKey t k)
              (\f -> liftIO (FDB.futureGetStringArray f) >>= liftFDBError)

-- TODO: rename to RangeQuery?
-- | Specifies a range of keys to be iterated over by 'getRange'.
data Range = Range {
  rangeBegin :: FDB.KeySelector
  -- ^ The beginning of the range, including the key specified by this
  -- 'KeySelector'.
  , rangeEnd :: FDB.KeySelector
  -- ^ The end of the range, not including the key specified by this
  -- 'KeySelector'.
  , rangeLimit :: Maybe Int
  -- ^ If the range contains more than @n@ items, return only @Just n@.
  -- If @Nothing@ is provided, returns the entire range.
  , rangeReverse :: Bool
  -- ^ If 'True', return the range in reverse order.
} deriving (Show, Eq, Ord)

-- | @keyRange begin end@ is the range of keys @[begin, end)@.
keyRange :: ByteString -> ByteString -> Range
keyRange begin end =
  Range (FDB.FirstGreaterOrEq begin) (FDB.FirstGreaterOrEq end) Nothing False

-- | @keyRange begin end@ is the range of keys @[begin, end]@.
keyRangeInclusive :: ByteString -> ByteString -> Range
keyRangeInclusive begin end =
  Range (FDB.FirstGreaterOrEq begin) (FDB.FirstGreaterThan end) Nothing False

-- | @prefixRange prefix@ is the range of all keys of which @prefix@ is a
--   prefix. Returns @Nothing@ if @prefix@ is empty or contains only @0xff@.
prefixRange :: ByteString -> Maybe Range
prefixRange prefix
  | BS.null prefix = Nothing
  | BS.all (== 0xff) prefix = Nothing
  | otherwise = Just $ Range
    { rangeBegin   = FDB.FirstGreaterOrEq prefix
    , rangeEnd     = FDB.FirstGreaterOrEq (prefixRangeEnd prefix)
    , rangeLimit   = Nothing
    , rangeReverse = False
    }

rangeKeys :: Range -> (ByteString, ByteString)
rangeKeys (Range b e _ _) = (FDB.keySelectorBytes b, FDB.keySelectorBytes e)

-- | Structure for returning the result of 'getRange' in chunks.
data RangeResult =
  RangeDone (Seq (ByteString, ByteString))
  | RangeMore (Seq (ByteString, ByteString)) (Future RangeResult)
  deriving Show

-- | Like 'getRange', but allows you to specify the streaming mode as desired.
getRange' :: Range -> FDB.FDBStreamingMode -> Transaction (Future RangeResult)
getRange' Range {..} mode = do
  t <- asks cTransaction
  isSnapshot <- asks (snapshotReads . envConf)
  let getR b e lim i = FDB.transactionGetRange t b e lim 0 mode i isSnapshot rangeReverse
  let mk = getR rangeBegin rangeEnd (fromMaybe 0 rangeLimit) 1
  let
    handler bsel esel i lim fut = do
      -- more doesn't take into account our count limit, so we check below
      (kvs, more) <- liftIO (FDB.futureGetKeyValueArray fut) >>= liftFDBError
      let kvs' = Seq.fromList kvs
      case kvs' of
        (_ :|> (lstK,_)) | more && maybe True (length kvs' <) lim -> do
          let bsel' = if not rangeReverse then FDB.FirstGreaterThan lstK else bsel
          let esel' = if rangeReverse then FDB.FirstGreaterOrEq lstK else esel
          let lim' = fmap (\x -> x - length kvs') lim
          let mk' = getR bsel' esel' (fromMaybe 0 lim') (i+1)
          res <- allocFuture mk' (handler bsel' esel' (i + 1) lim')
          return $ RangeMore kvs' res
        _ -> return $ RangeDone $ case lim of
          Nothing -> kvs'
          Just n  -> Seq.take n kvs'
  allocFuture mk (handler rangeBegin rangeEnd 1 rangeLimit)

-- | Reads all key-value pairs in the specified 'Range' which are
--   lexicographically greater than or equal to the 'rangeBegin' 'KeySelector'
--   and lexicographically less than the 'rangeEnd' 'KeySelector'.
--   Uses 'StreamingModeIterator', which assumes that you don't know ahead of
--   time exactly how many pairs in the range you actually need. If you need
--   them all (and they are expected to fit in memory), use 'getEntireRange'.
--   For more advanced usage, use 'getRange''.
getRange :: Range -> Transaction (Future RangeResult)
getRange r = getRange' r FDB.StreamingModeIterator

getEntireRange' :: FDB.FDBStreamingMode
                -> Range
                -> Transaction (Seq (ByteString, ByteString))
getEntireRange' mode r = do
  rr <- getRange' r mode >>= await
  go rr
 where
  go (RangeDone xs    ) = return xs
  go (RangeMore xs fut) = do
    more <- await fut
    ys   <- go more
    return (xs <> ys)

-- | Wrapper around 'getRange' that reads the entire range into memory.
getEntireRange :: Range -> Transaction (Seq (ByteString, ByteString))
getEntireRange = getEntireRange' FDB.StreamingModeWantAll

-- | Return True iff the given range is empty.
isRangeEmpty :: Range -> Transaction Bool
isRangeEmpty r = do
  rr <- getRange r >>= await
  case rr of
    RangeDone Empty -> return True
    _               -> return False

-- | Perform an atomic operation of 'MutationType' on the given key. A
-- transaction that performs only atomic operations is guaranteed not to
-- conflict. However, it may cause other concurrent transactions to conflict.
atomicOp :: ByteString -> Opt.MutationType -> Transaction ()
atomicOp k op = do
  t <- asks cTransaction
  liftIO $ FDB.transactionAtomicOp t k op

-- | Attempts to commit a transaction against the given database. If an
-- unretryable error occurs, throws an 'Error'. Attempts to retry the
-- transaction for retryable errors.
runTransaction :: FDB.Database -> Transaction a -> IO a
runTransaction = runTransactionWithConfig defaultConfig

-- | Like 'runTransaction', but returns a sum instead of throwing an exception
-- on errors.
runTransaction' :: FDB.Database -> Transaction a -> IO (Either Error a)
runTransaction' = runTransactionWithConfig' defaultConfig

-- | A config for a non-idempotent transaction, allowing 5 retries, with a time
-- limit of 500 milliseconds.
defaultConfig :: TransactionConfig
defaultConfig = TransactionConfig False False 5 500

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
  , maxRetries :: Int
  -- ^ Max number of times to retry retryable errors. After this many retries,
  -- 'MaxRetriesExceeded' will be thrown to the caller of 'runTransaction'.
  , timeout :: Int
  -- ^ Max number of milliseconds the transaction is allowed to run. If this
  -- number is exceeded, the transaction fails with an error.
  } deriving (Show, Read, Eq, Ord)

-- | Attempt to commit a transaction against the given database. If an
-- unretryable error occurs, throws an 'Error'. Attempts to retry the
-- transaction for retryable errors.
runTransactionWithConfig
  :: TransactionConfig -> FDB.Database -> Transaction a -> IO a
runTransactionWithConfig conf db t = do
  res <- runTransactionWithConfig' conf db t
  case res of
    Left  err -> throwIO err
    Right x   -> return x

-- | Handles the retry logic described in the FDB docs.
-- https://apple.github.io/foundationdb/api-c.html#c.fdb_transaction_on_error
withRetry :: Transaction a -> Transaction a
withRetry t = catchError t $ \err -> do
  idem <- asks (idempotent . envConf)
  retriesRemaining <- asks (maxRetries . envConf)
  let shouldRetry = if idem then retryable else retryableNotCommitted
  if shouldRetry err && retriesRemaining > 0
    then do
      onError err
      -- onError re-throws unretryable errors, so if we reach here, we can retry
      local (\e -> e {envConf = (envConf e) {maxRetries = retriesRemaining - 1} })
            (withRetry t)
    else if retriesRemaining == 0
            then throwError $ Error $ MaxRetriesExceeded err
            else throwError err

-- Attempt to commit a transaction against the given database. If an unretryable
-- error occurs, returns 'Left'. Attempts to retry the transaction for retryable
-- errors.
runTransactionWithConfig'
  :: TransactionConfig -> FDB.Database -> Transaction a -> IO (Either Error a)
runTransactionWithConfig' conf db t = runResourceT $ runExceptT $ do
  trans <- createTransactionEnv db conf
  flip runReaderT trans $ unTransaction $ withRetry $ do
    setOption (Opt.timeout (timeout conf))
    res    <- t
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

-- | Reset the transaction. All operations prior to this will be discarded.
reset :: Transaction ()
reset = do
  t <- asks cTransaction
  liftIO $ FDB.transactionReset t

-- | Runs a transaction using snapshot reads, which means that the transaction
-- will see the results of concurrent transactions, removing the default
-- serializable isolation guarantee.
withSnapshot :: Transaction a -> Transaction a
withSnapshot = local $ \s ->
  TransactionEnv (cTransaction s) ((envConf s) { snapshotReads = True })

-- | Sets the read version on the current transaction. As the FoundationDB docs
-- state, "this is not needed in simple cases".
setReadVersion :: Word64 -> Transaction ()
setReadVersion v = do
  t <- asks cTransaction
  liftIO $ FDB.transactionSetReadVersion t (fromIntegral v)

-- | Gets the read version of the current transaction, representing all
-- transactions that were reported committed before this one.
getReadVersion :: Transaction (Future Word64)
getReadVersion = do
  t <- asks cTransaction
  allocFuture (FDB.transactionGetReadVersion t)
              (\f -> fromIntegral <$> fdbExcept (FDB.futureGetVersion f))

-- | Returns a 'FutureIO' that will resolve to the versionstamp of the committed
-- transaction. Most applications won't need this.
getVersionstamp
  :: Transaction (FutureIO (Either Error TransactionVersionstamp))
getVersionstamp = do
  t <- asks cTransaction
  f <- liftIO $ FDB.transactionGetVersionstamp t
  liftIO $ allocFutureIO f $ FDB.futureGetKey f >>= \case
    Left  err -> return $ Left (CError $ fromJust $ toError err)
    Right bs  -> case decodeTransactionVersionstamp bs of
      Nothing ->
        return $
        Left $
        Error $
        ParseError $ "Failed to parse versionstamp: "
                     ++ show (BS.unpack bs)
      Just vs -> return $ Right vs

-- | Creates a future that will be fulfilled when the value
-- associated with the given key is changed, relative to the value
-- it had as of the current transaction's read version, or the last
-- value to which the key was previously set within the current
-- transaction. This future is safe to return from the transaction
-- and await in IO. If the transaction in which it was created
-- fails to commit, awaiting it will return the same error as
-- running the transaction did.
watch :: ByteString -> Transaction (FutureIO ())
watch k = do
  t <- asks cTransaction
  f <- liftIO $ FDB.transactionWatch t k
  liftIO $ allocFutureIO f $ return ()


-- | Set one of the transaction options from the underlying C API.
setOption :: Opt.TransactionOption -> Transaction ()
setOption opt = do
  t <- asks cTransaction
  fdbExcept' $ FDB.transactionSetOption t opt

{- $advanced
   The functionality in this section is for more advanced use cases where you
   need to be able to refer to an in-progress transaction and add operations to
   it incrementally. This is similar to how the Python's bindings work -- you
   pass around a transaction object and call methods on it one by one before
   finally calling @.commit()@.

   This functionality was needed to create the bindings tester, which is
   required to follow the semantics of the bindings for imperative languages
   more closely. You probably don't need this. In fact, it's not entirely clear
   that the bindings tester needs it.
-}

-- | The internal state of a transaction as it is being executed by
-- 'runTransaction'.
data TransactionEnv = TransactionEnv {
  cTransaction :: FDB.Transaction
  , envConf :: TransactionConfig
  } deriving (Show)

createTransactionEnv
  :: FDB.Database
  -> TransactionConfig
  -> ExceptT Error (ResourceT IO) TransactionEnv
createTransactionEnv db config = do
  (_rk, eTrans) <- allocate
    (fdbEither $ FDB.databaseCreateTransaction db)
    (either (const $ return ()) FDB.transactionDestroy)
  liftEither $ fmap (flip TransactionEnv config) eTrans

-- | Execute a transactional action on an existing transaction environment.
onEnv :: TransactionEnv -> Transaction a -> IO (Either Error a)
onEnv env (Transaction t) = runResourceT $ runExceptT $ runReaderT t env

-- | Calls the C API's @fdb_transaction_on_error@ function. Re-raises
-- unretryable errors.
onError :: Error -> Transaction ()
onError (CError err) = do
  trans <- asks cTransaction
  f     <- allocFuture (FDB.transactionOnError trans (toCFDBError err))
                       (const $ return ())
  await f
onError _ = return ()

-- @prefixRangeEnd prefix@ returns the lexicographically greatest bytestring
-- of which @prefix@ is a prefix. Usually, it's easier to just use
-- 'prefixRange'.
prefixRangeEnd :: ByteString -> ByteString
prefixRangeEnd prefix =
  let prefix' = BS.takeWhile (/= 0xff) prefix
  in  BS.snoc (BS.init prefix') (BS.last prefix' + 1)

-- | Gets the committed version of a transaction. Can only be called after the
-- transaction has committed, so must be used in conjunction with
-- 'TransactionEnv', since 'runTransaction' and its variants immediately destroy
-- the internal 'TransactionEnv' as soon as they return.
getCommittedVersion :: Transaction Int
getCommittedVersion = do
  t <- asks cTransaction
  fdbExcept (FDB.transactionGetCommittedVersion t)
