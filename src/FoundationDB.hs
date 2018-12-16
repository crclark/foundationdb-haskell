-- | WIP interface for constructing and running transactions.



{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module FoundationDB (
  -- * Initialization
  FDB.currentAPIVersion
  , withFoundationDB
  , FDB.Database
  -- * Transactions
  , Transaction
  , runTransaction
  , runTransaction'
  , TransactionConfig (..)
  , runTransactionWithConfig
  , runTransactionWithConfig'
  , cancel
  , reset
  , withSnapshot
  , setOption
  , setReadVersion
  , getReadVersion
  , getVersionstamp
  , get
  , set
  , clear
  , clearRange
  , addConflictRange
  , FDBConflictRangeType (..)
  , getKey
  , getKeyAddresses
  , atomicOp
  , getRange
  , getRange'
  , FDBStreamingMode
  , getEntireRange
  , isRangeEmpty
  , Range (..)
  , rangeKeys
  , keyRange
  , keyRangeInclusive
  , prefixRange
  , RangeResult (..)
  -- * Futures
  , Future
  , await
  , FutureIO
  , awaitIO
  -- * Key selectors
  , FDB.KeySelector( LastLessThan
                   , LastLessOrEq
                   , FirstGreaterThan
                   , FirstGreaterOrEq)
  , offset
  -- * Atomic operations
  , AtomicOp(..)
  -- * Errors
  , Error(..)
  , CError(..)
  , retryable
  -- * Helpers for ghci
  , startFoundationDB
  , stopFoundationDB
) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar, MVar)
import Control.Exception
import Control.Monad.Except
import Data.Maybe (fromMaybe)

import FoundationDB.Error
import qualified FoundationDB.Internal.Bindings as FDB
import FoundationDB.Transaction
import System.IO.Unsafe (unsafePerformIO)


initCluster :: FilePath -> IO (Either Error FDB.Cluster)
initCluster fp = do
  futureCluster <- FDB.createCluster fp
  runExceptT $ do
    fdbExcept' $ FDB.futureBlockUntilReady futureCluster
    fdbExcept $ FDB.futureGetCluster futureCluster

withCluster :: Maybe FilePath -> (Either Error FDB.Cluster -> IO a) -> IO a
withCluster mfp =
  bracket (initCluster (fromMaybe "" mfp))
          (either (const (return ())) FDB.clusterDestroy)

initDB :: FDB.Cluster -> IO (Either Error FDB.Database)
initDB cluster = do
  futureDB <- FDB.clusterCreateDatabase cluster
  runExceptT $ do
    fdbExcept' $ FDB.futureBlockUntilReady futureDB
    fdbExcept $ FDB.futureGetDatabase futureDB

withDatabase :: Maybe FilePath -> (Either Error FDB.Database -> IO a) -> IO a
withDatabase clusterFile f =
  withCluster clusterFile $ \case
    Left err -> f $ Left err
    Right cluster -> bracket (initDB cluster)
                             (either (const (return ())) FDB.databaseDestroy)
                             f

-- TODO: check that we support the desired API version and bail out otherwise.

-- | Handles correctly starting up the network connection to the DB.
-- Can only be called once per process!
withFoundationDB :: Int
                 -- ^ Desired API version. See 'currentAPIVersion' for the
                 -- latest version installed on your system.
                 -> Maybe FilePath
                 -- ^ Path to your @fdb.cluster@ file. If 'Nothing', uses
                 -- default location.
                 -> (Either Error FDB.Database -> IO a)
                 -> IO a
withFoundationDB version clusterFile m = do
  done <- newEmptyMVar
  fdbThrowing $ FDB.selectAPIVersion version
  fdbThrowing FDB.setupNetwork
  start done
  finally (withDatabase clusterFile m) (stop done)
  where
    start done = void $ forkFinally FDB.runNetwork (\_ -> putMVar done ())
    stop done = FDB.stopNetwork >> takeMVar done

startFoundationDBGlobalLock :: MVar ()
startFoundationDBGlobalLock = unsafePerformIO newEmptyMVar
{-# NOINLINE startFoundationDBGlobalLock #-}

-- | Starts up FoundationDB. You must call 'stopFoundationDB' before your
-- program terminates. It's recommended that you use 'withFoundationDB' instead,
-- since it handles cleanup. This function is only intended to be used in GHCi.
-- Can only be called once per process!
startFoundationDB :: Int
                  -- ^ Desired API version.
                  -> Maybe FilePath
                  -- ^ Cluster file. 'Nothing' uses the default.
                  -> IO (Either Error FDB.Database)
startFoundationDB v mfp = do
  fdbThrowing $ FDB.selectAPIVersion v
  fdbThrowing FDB.setupNetwork
  void $ forkFinally FDB.runNetwork
                     (\_ -> putMVar startFoundationDBGlobalLock ())
  mcluster <- initCluster (fromMaybe "" mfp)
  case mcluster of
    Left e -> return $ Left e
    Right c -> do
      mdb <- initDB c
      case mdb of
        Left e -> return $ Left e
        Right db -> return $ Right db

stopFoundationDB :: IO ()
stopFoundationDB = FDB.stopNetwork >> takeMVar startFoundationDBGlobalLock
