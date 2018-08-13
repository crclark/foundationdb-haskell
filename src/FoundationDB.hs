-- | WIP interface for constructing and running transactions.



{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module FoundationDB (
  -- * Initialization
  FDB.currentAPIVersion
  , withFoundationDB
  , withDatabase
  , FDB.Database
  -- * Transactions
  , Transaction
  , runTransaction
  , runTransaction'
  , TransactionConfig (..)
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
  , FDBConflictRangeType (..)
  , getKey
  , getKeyAddresses
  , atomicOp
  , getRange
  , getEntireRange
  , isRangeEmpty
  , Range (..)
  , rangeKeys
  , prefixRange
  , RangeResult (..)
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



-- TODO: withFoundationDB $ withDatabase is ugly.

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
withDatabase fp f =
  withCluster fp $ \case
    Left err -> f $ Left err
    Right cluster -> bracket (initDB cluster)
                             (either (const (return ())) FDB.databaseDestroy)
                             f

-- | Handles correctly starting up the network connection to the DB.
-- Calls `fdb_select_api_version` with the latest API version,
-- runs `fdb_run_network` on a separate thread,
-- then runs the user-provided action. Finally, on shutdown, calls
-- `fdb_stop_network`, waits for `fdb_run_network` to return, then returns.
-- Once this action has finished, it is safe for the program to exit.
-- Can only be called once per program!
withFoundationDB :: Int
                 -- ^ Desired API version.
                 -> IO a
                 -> IO a
withFoundationDB version m = do
  done <- newEmptyMVar
  fdbThrowing $ FDB.selectAPIVersion version
  fdbThrowing FDB.setupNetwork
  start done
  finally m (stop done)
  where
    start done = void $ forkFinally FDB.runNetwork (\_ -> putMVar done ())
    stop done = FDB.stopNetwork >> takeMVar done

startFoundationDBGlobalLock :: MVar ()
startFoundationDBGlobalLock = unsafePerformIO $ newEmptyMVar
{-# NOINLINE startFoundationDBGlobalLock #-}

-- | Starts up FoundationDB. You must call 'stopFoundationDB' before your
-- program terminates. It's recommended that you use 'withFoundationDB' instead,
-- since it handles cleanup. This function is only intended to be used in GHCi.
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
