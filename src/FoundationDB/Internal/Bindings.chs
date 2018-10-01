{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module FoundationDB.Internal.Bindings (
  -- * API versioning
  currentAPIVersion
  , selectAPIVersion
  -- * Network
  , setupNetwork
  , runNetwork
  , stopNetwork
  , networkSetOption
  -- * Future
  , Future (..)
  , futureCancel
  , futureDestroy
  , futureDestroyPtr
  , futureBlockUntilReady
  , futureIsReady
  , futureReleaseMemory
  , futureGetError
  , futureGetVersion
  , futureGetKey
  , futureGetCluster
  , futureGetDatabase
  , futureGetValue
  , futureGetStringArray
  , FDBKeyValue (..)
  , futureGetKeyValueArray
  -- * Cluster
  , Cluster
  , createCluster
  , clusterDestroy
  , clusterCreateDatabase
  -- * Database
  , Database
  , databaseDestroy
  , databaseSetOption
  , databaseCreateTransaction
  -- * Transaction
  , Transaction
  , KeySelector (..)
  , keySelectorBytes
  , keySelectorTuple
  , transactionDestroy
  , transactionSetOption
  , transactionSetReadVersion
  , transactionGetReadVersion
  , transactionGet
  , transactionGetKey
  , transactionGetAddressesForKey
  , transactionGetRange
  , FDBStreamingMode (..)
  , transactionSet
  , transactionClear
  , transactionClearRange
  , transactionAtomicOp
  , FDBMutationType (..)
  , transactionCommit
  , transactionGetCommittedVersion
  , transactionGetVersionstamp
  , transactionWatch
  , transactionOnError
  , transactionReset
  , transactionCancel
  , transactionAddConflictRange
  , FDBConflictRangeType (..)
  -- * Error
  , CFDBError
  , isError
  , getError
  , errorPredicate
  , FDBErrorPredicate (..)
) where

import FoundationDB.Options

import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr
import Foreign.Storable
#include <fdbc_wrapper.h>

{#context prefix = "fdb"#}

newtype CFDBError = CFDBError {getCFDBError :: CInt}
  deriving (Show, Eq, Ord, Num)

-- | Return 'True' iff 'CFDBError' value is an error (non-zero).
isError :: CFDBError -> Bool
isError = (/=0) . getCFDBError

currentAPIVersion :: Int
currentAPIVersion = {#const FDB_API_VERSION#}

{#fun unsafe select_api_version as selectAPIVersion
  {`Int'} -> `CFDBError' CFDBError#}

{#fun unsafe setup_network as ^ {} -> `CFDBError' CFDBError#}

{#fun run_network as ^ {} -> `CFDBError' CFDBError#}

{#fun unsafe stop_network as ^ {} -> `CFDBError' CFDBError#}

{#fun unsafe network_set_option as networkSetOption_
  {`Int', id `Ptr CUChar', `Int'}
  -> `CFDBError' CFDBError#}

networkSetOption :: NetworkOption -> IO CFDBError
networkSetOption (NetworkOptionString enum str) =
  withCStringLen str $ \(arr, len) ->
    networkSetOption_ enum (castPtr arr) len
networkSetOption (NetworkOptionInt enum i) = alloca $ \iptr -> do
  poke iptr i
  networkSetOption_ enum (castPtr iptr) (sizeOf i)
networkSetOption (NetworkOptionBytes enum bs) =
  B.useAsCStringLen bs $ \(arr, len) ->
    networkSetOption_ enum (castPtr arr) len
networkSetOption (NetworkOptionFlag enum) =
  networkSetOption_ enum nullPtr 0

newtype Future a = Future (C2HSImp.Ptr (Future a))

deriving instance Show (Future a)
deriving instance Storable (Future a)

inFuture :: Future a -> Ptr ()
inFuture (Future x) = castPtr x

outFuture ::Ptr () -> Future a
outFuture p = Future (castPtr p)

{#pointer *FDBCluster as Cluster newtype #}

deriving instance Show Cluster
deriving instance Storable Cluster

{#fun unsafe future_cancel as ^ {inFuture `Future a'} -> `()'#}

{#fun unsafe future_destroy as ^ {inFuture `Future a'} -> `()'#}

foreign import ccall "fdbc_wrapper.h &fdb_future_destroy"
  futureDestroyPtr :: FunPtr (Ptr a -> IO ())

{#fun future_block_until_ready as ^
  {inFuture `Future a'} -> `CFDBError' CFDBError#}

{#fun unsafe future_is_ready as ^ {inFuture `Future a'} -> `Bool'#}

-- TODO future_set_callback? Haskell has lightweight threads, so might be easier
-- to just fork and block.

{#fun unsafe future_release_memory as ^ {inFuture `Future a'} -> `()'#}

{#fun unsafe future_get_error as ^
  {inFuture `Future a'} -> `CFDBError' CFDBError#}

peekIntegral :: (Integral a, Storable a, Num b) => Ptr a -> IO b
peekIntegral x = fmap fromIntegral $ peek x

peekBool :: Ptr CInt -> IO Bool
peekBool x = fmap (/= 0) $ peek x

{#fun unsafe future_get_version as ^
  {inFuture `Future Int64', alloca- `Int64' peekIntegral*}
  -> `CFDBError' CFDBError#}

{#fun unsafe future_get_key as futureGetKey_
  {inFuture `Future a', alloca- `Ptr CUChar' peek*, alloca- `Int' peekIntegral*}
  -> `CFDBError' CFDBError#}

-- Retrying is provided by a helper function:
-- https://apple.github.io/foundationdb/api-c.html#c.fdb_transaction_on_error
futureGetKey :: Future B.ByteString -> IO (Either CFDBError B.ByteString)
futureGetKey f = do
  (err, cs, l) <- futureGetKey_ f
  if isError err
    then return $ Left err
    else do bs <- B.packCStringLen (castPtr cs, l)
            return $ Right bs

{#fun unsafe future_get_cluster as ^
  {inFuture `Future Cluster', alloca- `Cluster' peek*} -> `CFDBError' CFDBError#}


{#pointer *FDBDatabase as Database newtype #}

deriving instance Show Database
deriving instance Storable Database

{#fun unsafe future_get_database as ^
  {inFuture `Future Database', alloca- `Database' peek*}
  -> `CFDBError' CFDBError#}

{#fun unsafe future_get_value as futureGetValue_
  {inFuture `Future a'
  , alloca- `Bool' peekBool*
  , alloca- `Ptr CUChar' peek*
  , alloca- `Int' peekIntegral*}
  -> `CFDBError' CFDBError#}

futureGetValue :: Future (Maybe B.ByteString)
               -> IO (Either CFDBError (Maybe B.ByteString))
futureGetValue f = do
  (err, present, outstr, outlen) <- futureGetValue_ f
  case (isError err, present) of
    (True, _) -> return $ Left err
    (False, False) -> return $ Right Nothing
    (False, True) -> do bstr <- B.packCStringLen (castPtr outstr, outlen)
                        return $ Right $ Just bstr

{#fun unsafe future_get_string_array as futureGetStringArray_
  {inFuture `Future a'
  , alloca- `Ptr (Ptr CChar)' peek*
  , alloca- `Int' peekIntegral*}
  -> `CFDBError' CFDBError#}

futureGetStringArray :: Future [B.ByteString]
                     -> IO (Either CFDBError [B.ByteString])
futureGetStringArray f = do
  (err, strs, numStrs) <- futureGetStringArray_ f
  if isError err
    then return $ Left err
    else do strList <- peekArray numStrs strs
            bstrs <- mapM B.packCString strList
            return $ Right bstrs

data FDBKeyValue = FDBKeyValue
  { key :: Ptr ()
  , key_length :: Int
  , value :: Ptr ()
  , value_length :: Int}
  deriving (Show, Eq)

instance Storable FDBKeyValue where
  sizeOf _ = 24
  alignment _ = 4
  peek p = do
    key <- peekByteOff p 0 :: IO (Ptr ())
    key_length <- fromIntegral <$> (peekByteOff p 8 :: IO CInt)
    value <- peekByteOff p 12 :: IO (C2HSImp.Ptr ())
    value_length <- fromIntegral <$> (peekByteOff p 20 :: IO C2HSImp.CInt)
    return FDBKeyValue{..}
  poke p FDBKeyValue{..} = do
    pokeByteOff p 0 (key :: (C2HSImp.Ptr ()))
    pokeByteOff p 8 (fromIntegral key_length :: C2HSImp.CInt)
    pokeByteOff p 12 (value :: C2HSImp.Ptr ())
    pokeByteOff p 20 (fromIntegral value_length :: C2HSImp.CInt)

packKeyValue :: FDBKeyValue -> IO (B.ByteString, B.ByteString)
packKeyValue FDBKeyValue{..} = do
  k <- B.packCStringLen (castPtr key, key_length)
  v <- B.packCStringLen (castPtr value, value_length)
  return (k,v)

peekCastFDBKeyValue :: Ptr (Ptr FDBKeyValue) -> IO (Ptr FDBKeyValue)
peekCastFDBKeyValue p = do
  p' <- peek p
  return p'

peekFDBBool :: Ptr CInt -> IO Bool
peekFDBBool p = peek (castPtr p)

foreign import ccall unsafe "FoundationDB/Internal/Bindings.chs.h fdb_future_get_keyvalue_array"
  futureGetKeyValueArray_'_ :: ((C2HSImp.Ptr ()) -> ((C2HSImp.Ptr (C2HSImp.Ptr FDBKeyValue)) -> ((C2HSImp.Ptr C2HSImp.CInt) -> ((C2HSImp.Ptr C2HSImp.CInt) -> (IO C2HSImp.CInt)))))

futureGetKeyValueArray_ :: (Future a) -> IO ((CFDBError), (Ptr FDBKeyValue), (Int), (Bool))
futureGetKeyValueArray_ a1 =
  let {a1' = inFuture a1} in
  alloca $ \a2' ->
  alloca $ \a3' ->
  alloca $ \a4' ->
  futureGetKeyValueArray_'_ a1' a2' a3' a4' >>= \res ->
  let {res' = CFDBError res} in
  peekCastFDBKeyValue  a2'>>= \a2'' ->
  peekIntegral  a3'>>= \a3'' ->
  peekFDBBool  a4'>>= \a4'' ->
  return (res', a2'', a3'', a4'')

futureGetKeyValueArray :: Future [(B.ByteString, B.ByteString)]
                       -> IO (Either CFDBError
                                     ([(B.ByteString, B.ByteString)], Bool))
futureGetKeyValueArray f = do
  (err, arr, n, more) <- futureGetKeyValueArray_ f
  if isError err
    then return $ Left err
    else do kvs <- peekArray n arr >>= mapM packKeyValue
            return $ Right $ (kvs, more)

-- | If empty string is given for FilePath, tries to use the default cluster
-- file.
{#fun unsafe create_cluster as ^
  {withCString* `FilePath'} -> `Future Cluster' outFuture #}

{#fun unsafe cluster_destroy as ^ {`Cluster'} -> `()'#}

-- TODO: cluster_set_option.

{#fun unsafe cluster_create_database as clusterCreateDatabase_
  {`Cluster', id `Ptr CUChar', `Int'} -> `Future a' outFuture #}

clusterCreateDatabase :: Cluster -> IO (Future Database)
clusterCreateDatabase cluster =
  withCStringLen "DB" $ \(arr,len) ->
    clusterCreateDatabase_ cluster (castPtr arr) len

{#fun unsafe database_destroy as ^ {`Database'} -> `()'#}

{#fun unsafe database_set_option as databaseSetOption_
  {`Database', `Int', id `Ptr CUChar', `Int'}
  -> `CFDBError' CFDBError#}

databaseSetOption :: Database -> DatabaseOption -> IO CFDBError
databaseSetOption db (DatabaseOptionString enum str) =
  withCStringLen str $ \(arr,len) ->
    databaseSetOption_ db enum (castPtr arr) len
databaseSetOption db (DatabaseOptionInt enum i) = alloca $ \iptr -> do
  poke iptr i
  databaseSetOption_ db enum (castPtr iptr) 8
databaseSetOption db (DatabaseOptionBytes enum bs) =
  B.useAsCStringLen bs $ \(arr, len) ->
    databaseSetOption_ db enum (castPtr arr) len
databaseSetOption db (DatabaseOptionFlag enum) =
  databaseSetOption_ db enum nullPtr 0

{#pointer *FDBTransaction as Transaction newtype #}

deriving instance Show Transaction
deriving instance Storable Transaction

{#fun unsafe database_create_transaction as ^
  {`Database', alloca- `Transaction' peek*} -> `CFDBError' CFDBError#}

{#fun unsafe transaction_destroy as ^ {`Transaction'} -> `()'#}

{#fun unsafe transaction_set_option as transactionSetOption_
  {`Transaction', `Int', id `Ptr CUChar', `Int'}
  -> `CFDBError' CFDBError #}

transactionSetOption :: Transaction -> TransactionOption -> IO CFDBError
transactionSetOption t (TransactionOptionString enum str) =
  withCStringLen str $ \(arr, len) ->
    transactionSetOption_ t enum (castPtr arr) len
transactionSetOption t (TransactionOptionInt enum i) = alloca $ \iptr ->
  do poke iptr i
     transactionSetOption_ t enum (castPtr iptr) (sizeOf i)
transactionSetOption t (TransactionOptionBytes enum bs) =
  B.useAsCStringLen bs $ \(arr, len) ->
    transactionSetOption_ t enum (castPtr arr) len
transactionSetOption t (TransactionOptionFlag enum) =
  transactionSetOption_ t enum nullPtr 0

{#fun unsafe transaction_get_read_version as ^
  {`Transaction'} -> `Future Int64' outFuture #}

{#fun unsafe transaction_set_read_version as ^
  {`Transaction', `Int64'} -> `()'#}

{#fun unsafe transaction_get as transactionGet_
  {`Transaction', id `Ptr CUChar', `Int', `Bool'} -> `Future a' outFuture #}

transactionGet :: Transaction
               -> B.ByteString
               -> Bool
               -> IO (Future (Maybe B.ByteString))
transactionGet t k isSnapshotRead = B.useAsCStringLen k $ \(kstr,klen) ->
  transactionGet_ t (castPtr kstr) klen isSnapshotRead

{#fun unsafe transaction_get_key as transactionGetKey_
  {`Transaction', id `Ptr CUChar', `Int', `Bool', `Int', `Bool'}
  -> `Future a' outFuture #}

data KeySelector =
  LastLessThan B.ByteString
  | LastLessOrEq B.ByteString
  | FirstGreaterThan B.ByteString
  | FirstGreaterOrEq B.ByteString
  | WithOffset Int KeySelector
  deriving (Show, Eq, Ord)

keySelectorBytes :: KeySelector -> B.ByteString
keySelectorBytes (LastLessThan bs) = bs
keySelectorBytes (LastLessOrEq bs) = bs
keySelectorBytes (FirstGreaterThan bs) = bs
keySelectorBytes (FirstGreaterOrEq bs) = bs
keySelectorBytes (WithOffset _ ks) = keySelectorBytes ks

-- | Convert a 'KeySelector' to its or_equal, offset settings. Equivalent to
-- the macros @FDB_KEYSEL_LAST_LESS_THAN@ etc.
keySelectorTuple :: KeySelector -> (B.ByteString, Bool, Int)
keySelectorTuple (LastLessThan bs) = (bs, False, 0)
keySelectorTuple (LastLessOrEq bs) = (bs, True, 0)
keySelectorTuple (FirstGreaterThan bs) = (bs, True, 1)
keySelectorTuple (FirstGreaterOrEq bs) = (bs, False, 1)
keySelectorTuple (WithOffset n ks) =
  (\(x,y,z) -> (x, y, z+n)) (keySelectorTuple ks)

transactionGetKey :: Transaction
                  -> B.ByteString
                  -> Bool
                  -> Int
                  -> Bool
                  -> IO (Future B.ByteString)
transactionGetKey t k orEqual offset isSnapshotRead =
  B.useAsCStringLen k $ \(kstr, klen) ->
    transactionGetKey_ t (castPtr kstr) klen orEqual offset isSnapshotRead

{#fun unsafe transaction_get_addresses_for_key
  as transactionGetAddressForKey_
  {`Transaction', id `Ptr CUChar', `Int'}
  -> `Future a' outFuture #}

transactionGetAddressesForKey :: Transaction
                              -> B.ByteString
                              -> IO (Future [B.ByteString])
transactionGetAddressesForKey t k = B.useAsCStringLen k $ \(kstr, klen) ->
  transactionGetAddressForKey_ t (castPtr kstr) klen

{#enum FDBStreamingMode {underscoreToCase}#}

deriving instance Eq FDBStreamingMode
deriving instance Ord FDBStreamingMode
deriving instance Show FDBStreamingMode

{#fun unsafe transaction_get_range as transactionGetRange_
  {`Transaction'
  , id `Ptr CUChar', `Int', `Bool', `Int'
  , id `Ptr CUChar', `Int', `Bool', `Int'
  , `Int'
  , `Int'
  , `FDBStreamingMode'
  , `Int'
  , `Bool'
  , `Bool'}
  -> `Future a' outFuture #}

transactionGetRange :: Transaction
                    -> B.ByteString
                    -- ^ Begin key
                    -> Bool
                    -- ^ Begin key orEqual
                    -> Int
                    -- ^ Begin key offset
                    -> B.ByteString
                    -- ^ end key
                    -> Bool
                    -- ^ end key orEqual
                    -> Int
                    -- ^ end key offset
                    -> Int
                    -- ^ max number of pairs to return
                    -> Int
                    -- ^ max number of bytes to return
                    -> FDBStreamingMode
                    -> Int
                    -- ^ if FDBStreamingMode is FdbStreamingModeIterator,
                    -- start this at 1 and increment by for each successive
                    -- call reading this range. Otherwise, ignored.
                    -> Bool
                    -- ^ isSnapshotRead
                    -> Bool
                    -- ^ whether to return pairs in reverse order
                    -> IO (Future [(B.ByteString, B.ByteString)])
transactionGetRange t bk bOrEqual bOffset
                      ek eOrEqual eOffset
                      pairLimit byteLimit
                      streamMode iteratorI
                      isSnapshotRead isReverse =
  B.useAsCStringLen bk $ \(bstr, blen) ->
  B.useAsCStringLen ek $ \(estr, elen) ->
  transactionGetRange_ t
                       (castPtr bstr) blen bOrEqual bOffset
                       (castPtr estr) elen eOrEqual eOffset
                       pairLimit byteLimit
                       streamMode iteratorI
                       isSnapshotRead isReverse

{#fun unsafe transaction_set as transactionSet_
  {`Transaction'
  , id `Ptr CUChar', `Int'
  , id `Ptr CUChar', `Int'}
  -> `()'#}

transactionSet :: Transaction -> B.ByteString -> B.ByteString -> IO ()
transactionSet t k v =
  B.useAsCStringLen k $ \(kstr, klen) ->
  B.useAsCStringLen v $ \(vstr, vlen) ->
  transactionSet_ t (castPtr kstr) klen (castPtr vstr) vlen

{#fun unsafe transaction_clear as transactionClear_
  {`Transaction', id `Ptr CUChar', `Int'}
  -> `()'#}

transactionClear :: Transaction -> B.ByteString -> IO ()
transactionClear t k = B.useAsCStringLen k $ \(kstr, klen) ->
  transactionClear_ t (castPtr kstr) klen

{# fun unsafe transaction_clear_range as transactionClearRange_
  {`Transaction'
  , id `Ptr CUChar', `Int'
  , id `Ptr CUChar', `Int'}
  -> `()'#}

transactionClearRange :: Transaction -> B.ByteString -> B.ByteString -> IO ()
transactionClearRange t greaterOrEqBound lessThanBound =
  B.useAsCStringLen greaterOrEqBound $ \(minstr, minlen) ->
  B.useAsCStringLen lessThanBound $ \(maxstr, maxlen) ->
  transactionClearRange_ t (castPtr minstr) minlen (castPtr maxstr) maxlen

{#enum FDBMutationType {underscoreToCase}#}

deriving instance Eq FDBMutationType
deriving instance Ord FDBMutationType
deriving instance Show FDBMutationType

{#fun unsafe transaction_atomic_op as transactionAtomicOp_
  {`Transaction'
  , id `Ptr CUChar', `Int'
  , id `Ptr CUChar', `Int'
  , `FDBMutationType'}
  -> `()'#}

transactionAtomicOp :: Transaction
                    -> B.ByteString
                    -> B.ByteString
                    -> FDBMutationType
                    -> IO ()
transactionAtomicOp t k arg mutation =
  B.useAsCStringLen k $ \(kstr, klen) ->
  B.useAsCStringLen arg $ \(argstr, arglen) ->
  transactionAtomicOp_ t (castPtr kstr) klen (castPtr argstr) arglen mutation

{#fun unsafe transaction_commit as ^
  {`Transaction'} -> `Future ()' outFuture #}

{#fun unsafe transaction_get_committed_version as ^
  {`Transaction', alloca- `Int'} -> `CFDBError' CFDBError#}

{#fun unsafe transaction_get_versionstamp as ^
  {`Transaction'} -> `Future B.ByteString' outFuture #}

{#fun unsafe transaction_watch as transactionWatch_
  {`Transaction', id `Ptr CUChar', `Int'}
  -> `Future a' outFuture #}

transactionWatch :: Transaction -> B.ByteString -> IO (Future ())
transactionWatch t k = B.useAsCStringLen k $ \(kstr, klen) ->
  transactionWatch_ t (castPtr kstr) klen

{#fun unsafe transaction_on_error as ^
  {`Transaction', getCFDBError `CFDBError'}
  -> `Future ()' outFuture #}

{#fun unsafe transaction_reset as ^
  {`Transaction'} -> `()'#}

{#fun unsafe transaction_cancel as ^
  {`Transaction'} -> `()'#}

{#enum FDBConflictRangeType {underscoreToCase}#}

deriving instance Eq FDBConflictRangeType
deriving instance Ord FDBConflictRangeType
deriving instance Show FDBConflictRangeType

{#fun unsafe transaction_add_conflict_range as transactionAddConflictRange_
  {`Transaction'
  , id `Ptr CUChar', `Int'
  , id `Ptr CUChar', `Int'
  , `FDBConflictRangeType'}
  -> `CFDBError' CFDBError#}

transactionAddConflictRange :: Transaction
                            -> B.ByteString
                            -> B.ByteString
                            -> FDBConflictRangeType
                            -> IO CFDBError
transactionAddConflictRange t begin end conflictRangeType =
  B.useAsCStringLen begin $ \(beginstr, beginlen) ->
  B.useAsCStringLen end $ \(endstr, endlen) ->
  transactionAddConflictRange_ t
                               (castPtr beginstr) beginlen
                               (castPtr endstr) endlen
                               conflictRangeType

{#fun unsafe get_error as ^ {getCFDBError `CFDBError'} -> `String'#}

{#enum FDBErrorPredicate {underscoreToCase}#}

deriving instance Eq FDBErrorPredicate
deriving instance Ord FDBErrorPredicate
deriving instance Show FDBErrorPredicate

{#fun pure unsafe error_predicate as ^
  {`FDBErrorPredicate', getCFDBError `CFDBError'}
  -> `Bool'#}
