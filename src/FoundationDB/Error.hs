{-# LANGUAGE FlexibleContexts #-}

module FoundationDB.Error where

import Control.Exception
import Control.Monad.Error.Class (MonadError(..), liftEither)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromJust)

import qualified FoundationDB.Internal.Bindings as FDB

-- TODO: it's still unclear what facilities should be in Bindings and what
-- should be up here. 'fdbEither' and other helpers might work better if they
-- were built into the functions exported from Bindings.

fdbEither :: MonadIO m => m (FDB.CFDBError, a) -> m (Either Error a)
fdbEither f = do
  (err, res) <- f
  case toError err of
    Just x -> return $ Left $ CError x
    Nothing -> return (Right res)

fdbExcept :: (MonadError Error m, MonadIO m)
             => IO (FDB.CFDBError, a) -> m a
fdbExcept x = do
  e <- liftIO $ fdbEither x
  liftEither e

fdbEither' :: MonadIO m => m FDB.CFDBError -> m (Either Error ())
fdbEither' f = do
  err <- f
  case toError err of
    Just x -> return $ Left $ CError x
    Nothing -> return (Right ())

fdbExcept' :: (MonadError Error m, MonadIO m) =>
               IO FDB.CFDBError -> m ()
fdbExcept' x = do
  e <- liftIO $ fdbEither' x
  liftEither e

liftFDBError :: MonadError Error m => Either FDB.CFDBError a -> m a
liftFDBError = either (throwError . CError . fromJust . toError) return

fdbThrowing :: IO FDB.CFDBError -> IO ()
fdbThrowing a = do
  e <- a
  case toError e of
    Just x -> throwIO $ CError x
    Nothing -> return ()

data Error = CError CError | Error FDBHsError
  deriving (Show, Eq, Ord)

instance Exception Error

-- | Errors arising from the foundationdb-haskell library implementation.
data FDBHsError =
  DirectoryLayerError String
  | ParseError String
  deriving (Show, Eq, Ord)

-- | Errors that can come from the underlying C library.
-- Most error names are self-explanatory.
-- See https://apple.github.io/foundationdb/api-error-codes.html#developer-guide-error-codes
-- for a description of these errors.
data CError =
  OperationFailed
  | TimedOut
  | TransactionTooOld
  | FutureVersion
  | NotCommitted
  | CommitUnknownResult
  | TransactionCanceled
  | TransactionTimedOut
  | TooManyWatches
  | WatchesDisabled
  | AccessedUnreadable
  | DatabaseLocked
  | ClusterVersionChanged
  | ExternalClientAlreadyLoaded
  | OperationCancelled
  | FutureReleased
  | PlatformError
  | LargeAllocFailed
  | PerformanceCounterError
  | IOError
  | FileNotFound
  | BindFailed
  | FileNotReadable
  | FileNotWritable
  | NoClusterFileFound
  | FileTooLarge
  | ClientInvalidOperation
  | CommitReadIncomplete
  | TestSpecificationInvalid
  | KeyOutsideLegalRange
  | InvertedRange
  | InvalidOptionValue
  | InvalidOption
  | NetworkNotSetup
  | NetworkAlreadySetup
  | ReadVersionAlreadySet
  | VersionInvalid
  | RangeLimitsInvalid
  | InvalidDatabaseName
  | AttributeNotFound
  | FutureNotSet
  | FutureNotError
  | UsedDuringCommit
  | InvalidMutationType
  | TransactionInvalidVersion
  | TransactionReadOnly2021
  -- ^ this has the same name as error code 2023, hence the int suffix.
  | EnvironmentVariableNetworkOptionFailed
  | TransactionReadOnly2023
  | IncompatibleProtocolVersion
  | TransactionTooLarge
  | KeyTooLarge
  | ValueTooLarge
  | ConnectionStringInvalid
  | AddressInUse
  | InvalidLocalAddress
  | TLSError
  | UnsupportedOperation
  | APIVersionUnset
  | APIVersionAlreadySet
  | APIVersionInvalid
  | APIVersionNotSupported
  | ExactModeWithoutLimits
  | UnknownError
  | InternalError
  | OtherError {getOtherError :: FDB.CFDBError}
  deriving (Show, Eq, Ord)

-- | Convert error int to 'CError' sum. If 0 (which indicates success), returns
-- 'Nothing'. See 'isError' for another way to guard for success.
toError :: FDB.CFDBError -> Maybe CError
toError 0 = Nothing
toError 1000 = Just OperationFailed
toError 1004 = Just TimedOut
toError 1007 = Just TransactionTooOld
toError 1009 = Just FutureVersion
toError 1020 = Just NotCommitted
toError 1021 = Just CommitUnknownResult
toError 1025 = Just TransactionCanceled
toError 1031 = Just TransactionTimedOut
toError 1032 = Just TooManyWatches
toError 1034 = Just WatchesDisabled
toError 1036 = Just AccessedUnreadable
toError 1038 = Just DatabaseLocked
toError 1039 = Just ClusterVersionChanged
toError 1040 = Just ExternalClientAlreadyLoaded
toError 1101 = Just OperationCancelled
toError 1102 = Just FutureReleased
toError 1500 = Just PlatformError
toError 1501 = Just LargeAllocFailed
toError 1502 = Just PerformanceCounterError
toError 1510 = Just IOError
toError 1511 = Just FileNotFound
toError 1512 = Just BindFailed
toError 1513 = Just FileNotReadable
toError 1514 = Just FileNotWritable
toError 1515 = Just NoClusterFileFound
toError 1516 = Just FileTooLarge
toError 2000 = Just ClientInvalidOperation
toError 2002 = Just CommitReadIncomplete
toError 2003 = Just TestSpecificationInvalid
toError 2004 = Just KeyOutsideLegalRange
toError 2005 = Just InvertedRange
toError 2006 = Just InvalidOptionValue
toError 2007 = Just InvalidOption
toError 2008 = Just NetworkNotSetup
toError 2009 = Just NetworkAlreadySetup
toError 2010 = Just ReadVersionAlreadySet
toError 2011 = Just VersionInvalid
toError 2012 = Just RangeLimitsInvalid
toError 2013 = Just InvalidDatabaseName
toError 2014 = Just AttributeNotFound
toError 2015 = Just FutureNotSet
toError 2016 = Just FutureNotError
toError 2017 = Just UsedDuringCommit
toError 2018 = Just InvalidMutationType
toError 2020 = Just TransactionInvalidVersion
toError 2021 = Just TransactionReadOnly2021
toError 2022 = Just EnvironmentVariableNetworkOptionFailed
toError 2023 = Just TransactionReadOnly2023
toError 2100 = Just IncompatibleProtocolVersion
toError 2101 = Just TransactionTooLarge
toError 2102 = Just KeyTooLarge
toError 2103 = Just ValueTooLarge
toError 2104 = Just ConnectionStringInvalid
toError 2105 = Just AddressInUse
toError 2106 = Just InvalidLocalAddress
toError 2107 = Just TLSError
toError 2108 = Just UnsupportedOperation
toError 2200 = Just APIVersionUnset
toError 2201 = Just APIVersionAlreadySet
toError 2202 = Just APIVersionInvalid
toError 2203 = Just APIVersionNotSupported
toError 2210 = Just ExactModeWithoutLimits
toError 4000 = Just UnknownError
toError 4100 = Just InternalError
toError n = Just $ OtherError n

toCFDBError :: CError -> FDB.CFDBError
toCFDBError OperationFailed = 1000
toCFDBError TimedOut = 1004
toCFDBError TransactionTooOld = 1007
toCFDBError FutureVersion = 1009
toCFDBError NotCommitted = 1020
toCFDBError CommitUnknownResult = 1021
toCFDBError TransactionCanceled = 1025
toCFDBError TransactionTimedOut = 1031
toCFDBError TooManyWatches = 1032
toCFDBError WatchesDisabled = 1034
toCFDBError AccessedUnreadable = 1036
toCFDBError DatabaseLocked = 1038
toCFDBError ClusterVersionChanged = 1039
toCFDBError ExternalClientAlreadyLoaded = 1040
toCFDBError OperationCancelled = 1101
toCFDBError FutureReleased = 1102
toCFDBError PlatformError = 1500
toCFDBError LargeAllocFailed = 1501
toCFDBError PerformanceCounterError = 1502
toCFDBError IOError = 1510
toCFDBError FileNotFound = 1511
toCFDBError BindFailed = 1512
toCFDBError FileNotReadable = 1513
toCFDBError FileNotWritable = 1514
toCFDBError NoClusterFileFound = 1515
toCFDBError FileTooLarge = 1516
toCFDBError ClientInvalidOperation = 2000
toCFDBError CommitReadIncomplete = 2002
toCFDBError TestSpecificationInvalid = 2003
toCFDBError KeyOutsideLegalRange = 2004
toCFDBError InvertedRange = 2005
toCFDBError InvalidOptionValue = 2006
toCFDBError InvalidOption = 2007
toCFDBError NetworkNotSetup = 2008
toCFDBError NetworkAlreadySetup = 2009
toCFDBError ReadVersionAlreadySet = 2010
toCFDBError VersionInvalid = 2011
toCFDBError RangeLimitsInvalid = 2012
toCFDBError InvalidDatabaseName = 2013
toCFDBError AttributeNotFound = 2014
toCFDBError FutureNotSet = 2015
toCFDBError FutureNotError = 2016
toCFDBError UsedDuringCommit = 2017
toCFDBError InvalidMutationType = 2018
toCFDBError TransactionInvalidVersion = 2020
toCFDBError TransactionReadOnly2021 = 2021
toCFDBError EnvironmentVariableNetworkOptionFailed = 2022
toCFDBError TransactionReadOnly2023 = 2023
toCFDBError IncompatibleProtocolVersion = 2100
toCFDBError TransactionTooLarge = 2101
toCFDBError KeyTooLarge = 2102
toCFDBError ValueTooLarge = 2103
toCFDBError ConnectionStringInvalid = 2104
toCFDBError AddressInUse = 2105
toCFDBError InvalidLocalAddress = 2106
toCFDBError TLSError = 2107
toCFDBError UnsupportedOperation = 2108
toCFDBError APIVersionUnset = 2200
toCFDBError APIVersionAlreadySet = 2201
toCFDBError APIVersionInvalid = 2202
toCFDBError APIVersionNotSupported = 2203
toCFDBError ExactModeWithoutLimits = 2210
toCFDBError UnknownError = 4000
toCFDBError InternalError = 4100
toCFDBError (OtherError err) = err

retryable :: Error -> Bool
retryable (CError e) =
  FDB.errorPredicate FDB.ErrorPredicateRetryable (toCFDBError e)
retryable (Error _) = False

retryableNotCommitted :: Error -> Bool
retryableNotCommitted (CError e) =
  FDB.errorPredicate FDB.ErrorPredicateRetryableNotCommitted (toCFDBError e)
retryableNotCommitted (Error _) = False
