{-# LANGUAGE FlexibleContexts #-}

module FoundationDB.Error where

import Control.Exception
import Control.Monad
import Control.Monad.Error.Class (MonadError(..), liftEither)
import Control.Monad.IO.Class (MonadIO(..))

import qualified FoundationDB.Internal.Bindings as FDB

-- TODO: it's still unclear what facilities should be in Bindings and what
-- should be up here. 'fdbEither' and other helpers might work better if they
-- were built into the functions exported from Bindings.

fdbEither :: MonadIO m => m (FDB.CFDBError, a) -> m (Either Error a)
fdbEither f = do
  (err, res) <- f
  if FDB.isError err
    then return $ Left $ toError err
    else return (Right res)

fdbExcept :: (MonadError Error m, MonadIO m)
             => IO (FDB.CFDBError, a) -> m a
fdbExcept x = do
  e <- liftIO $ fdbEither x
  liftEither e

fdbEither' :: MonadIO m => m FDB.CFDBError -> m (Either Error ())
fdbEither' f = do
  err <- f
  if FDB.isError err
    then return $ Left $ toError err
    else return (Right ())

fdbExcept' :: (MonadError Error m, MonadIO m) =>
               IO FDB.CFDBError -> m ()
fdbExcept' x = do
  e <- liftIO $ fdbEither' x
  liftEither e

liftFDBError :: MonadError Error m => Either FDB.CFDBError a -> m a
liftFDBError = either (throwError . toError) return

fdbThrowing :: IO FDB.CFDBError -> IO ()
fdbThrowing a = do
  e <- a
  when (FDB.isError e) (throwIO (toError e))

-- | Errors that can come from the underlying C library.
-- Most error names are self-explanatory.
-- See https://apple.github.io/foundationdb/api-error-codes.html#developer-guide-error-codes
-- for a description of these errors.
data Error =
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

instance Exception Error

toError :: FDB.CFDBError -> Error
toError 0 = error "toError called on successful error code"
toError 1000 = OperationFailed
toError 1004 = TimedOut
toError 1007 = TransactionTooOld
toError 1009 = FutureVersion
toError 1020 = NotCommitted
toError 1021 = CommitUnknownResult
toError 1025 = TransactionCanceled
toError 1031 = TransactionTimedOut
toError 1032 = TooManyWatches
toError 1034 = WatchesDisabled
toError 1036 = AccessedUnreadable
toError 1038 = DatabaseLocked
toError 1039 = ClusterVersionChanged
toError 1040 = ExternalClientAlreadyLoaded
toError 1101 = OperationCancelled
toError 1102 = FutureReleased
toError 1500 = PlatformError
toError 1501 = LargeAllocFailed
toError 1502 = PerformanceCounterError
toError 1510 = IOError
toError 1511 = FileNotFound
toError 1512 = BindFailed
toError 1513 = FileNotReadable
toError 1514 = FileNotWritable
toError 1515 = NoClusterFileFound
toError 1516 = FileTooLarge
toError 2000 = ClientInvalidOperation
toError 2002 = CommitReadIncomplete
toError 2003 = TestSpecificationInvalid
toError 2004 = KeyOutsideLegalRange
toError 2005 = InvertedRange
toError 2006 = InvalidOptionValue
toError 2007 = InvalidOption
toError 2008 = NetworkNotSetup
toError 2009 = NetworkAlreadySetup
toError 2010 = ReadVersionAlreadySet
toError 2011 = VersionInvalid
toError 2012 = RangeLimitsInvalid
toError 2013 = InvalidDatabaseName
toError 2014 = AttributeNotFound
toError 2015 = FutureNotSet
toError 2016 = FutureNotError
toError 2017 = UsedDuringCommit
toError 2018 = InvalidMutationType
toError 2020 = TransactionInvalidVersion
toError 2021 = TransactionReadOnly2021
toError 2022 = EnvironmentVariableNetworkOptionFailed
toError 2023 = TransactionReadOnly2023
toError 2100 = IncompatibleProtocolVersion
toError 2101 = TransactionTooLarge
toError 2102 = KeyTooLarge
toError 2103 = ValueTooLarge
toError 2104 = ConnectionStringInvalid
toError 2105 = AddressInUse
toError 2106 = InvalidLocalAddress
toError 2107 = TLSError
toError 2108 = UnsupportedOperation
toError 2200 = APIVersionUnset
toError 2201 = APIVersionAlreadySet
toError 2202 = APIVersionInvalid
toError 2203 = APIVersionNotSupported
toError 2210 = ExactModeWithoutLimits
toError 4000 = UnknownError
toError 4100 = InternalError
toError n = OtherError n

toCFDBError :: Error -> FDB.CFDBError
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
retryable e =
  FDB.errorPredicate FDB.ErrorPredicateRetryable (toCFDBError e)

retryableNotCommitted :: Error -> Bool
retryableNotCommitted e =
  FDB.errorPredicate FDB.ErrorPredicateRetryableNotCommitted (toCFDBError e)
