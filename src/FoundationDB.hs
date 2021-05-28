-- | This module contains all of the basics needed to build a program that
-- interacts with <https://apple.github.io/foundationdb/index.html FoundationDB>.
-- The documentation throughout this library assumes that you have already read
-- the official
-- <https://apple.github.io/foundationdb/developer-guide.html developer guide>.
--
-- = Quick start
--
-- * @import qualified FoundationDB as FDB@
-- * Use 'withFoundationDB' to get a handle to the database.
-- * Use 'runTransaction' and its variants to run a transaction in the IO monad.
-- * Read the docs in "FoundationDB.Transaction" to learn how to use the
--   'Transaction' monad.
-- * 'runTransaction' throws exceptions. 'runTransaction'' returns a sum type.
--   Whichever you choose, all errors you can encounter are defined in
--   "FoundationDB.Error".
-- * See <https://github.com/crclark/foundationdb-haskell/blob/master/tests/Properties/FoundationDB/Transaction.hs#L48 the tests> for basic usage examples.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module FoundationDB (
  -- * Initialization
  FDB.currentAPIVersion
  , withFoundationDB
  , FoundationDBOptions(..)
  , defaultOptions
  , FDB.Database
  -- * Transactions
  , module FoundationDB.Transaction
  -- * Errors
  , module FoundationDB.Error
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
import FoundationDB.Error.Internal
import qualified FoundationDB.Internal.Bindings as FDB
import FoundationDB.Options.DatabaseOption (DatabaseOption(..))
import FoundationDB.Options.NetworkOption (NetworkOption(..))
import FoundationDB.Transaction
import System.IO.Unsafe (unsafePerformIO)

-- | This library doesn't support FDB versions earlier than 5.2 (the first
-- open source release).
validateVersion :: Int -> IO ()
validateVersion v =
  when (v < 520)
       (throw (Error UnsupportedAPIVersion))

#if FDB_API_VERSION < 610
initCluster :: FilePath -> IO FDB.Cluster
initCluster fp = do
  futureCluster <- FDB.createCluster fp
  fdbThrowing' $ FDB.futureBlockUntilReady futureCluster
  fdbThrowing $ FDB.futureGetCluster futureCluster

withCluster :: Maybe FilePath -> (FDB.Cluster -> IO a) -> IO a
withCluster mfp =
  bracket (initCluster (fromMaybe "" mfp))
          FDB.clusterDestroy

initDB :: FDB.Cluster -> IO FDB.Database
initDB cluster = do
  futureDB <- FDB.clusterCreateDatabase cluster
  fdbThrowing' $ FDB.futureBlockUntilReady futureDB
  fdbThrowing $ FDB.futureGetDatabase futureDB

withDatabase :: Maybe FilePath -> (FDB.Database -> IO a) -> IO a
withDatabase clusterFile f =
  withCluster clusterFile $ \ cluster ->
    bracket (initDB cluster)
            FDB.databaseDestroy
            f
#else
withDatabase :: Maybe FilePath -> (FDB.Database -> IO a) -> IO a
withDatabase clusterFile =
  bracket (fdbThrowing $ FDB.createDatabase (fromMaybe "" clusterFile))
          FDB.databaseDestroy
#endif

-- | Options set at the connection level for FoundationDB.
data FoundationDBOptions = FoundationDBOptions
  { apiVersion :: Int
    -- ^ Desired API version. See 'currentAPIVersion' for the latest
    -- version installed on your system.
  , clusterFile :: Maybe FilePath
  -- ^ Path to your @fdb.cluster@ file. If 'Nothing', uses
  -- default location.
  , networkOptions :: [NetworkOption]
  -- ^ Additional network options. Each will be set in order.
  , databaseOptions :: [DatabaseOption]
  -- ^ Additional database options. Each will be set in order.
  } deriving (Show, Eq, Ord)

-- | Uses the current API version, the default cluster file location, and no
-- additional options.
defaultOptions :: FoundationDBOptions
defaultOptions = FoundationDBOptions FDB.currentAPIVersion Nothing [] []

-- | Handles correctly starting up the network connection to the DB.
-- Can only be called once per process! Throws an 'Error' if any part of
-- setting up the connection to FoundationDB fails.
withFoundationDB :: FoundationDBOptions
                 -> (FDB.Database -> IO a)
                 -> IO a
withFoundationDB FoundationDBOptions{..} m = do
  validateVersion apiVersion
  done <- newEmptyMVar
  fdbThrowing' $ FDB.selectAPIVersion apiVersion
  forM_ networkOptions (fdbThrowing' . FDB.networkSetOption)
  fdbThrowing' FDB.setupNetwork
  start done
  finally (withDatabase clusterFile run) (stop done)
  where
    start done = void $ forkFinally FDB.runNetwork (\_ -> putMVar done ())
    stop done = FDB.stopNetwork >> takeMVar done
    run db = do
        forM_ databaseOptions (fdbThrowing' . FDB.databaseSetOption db)
        m db

startFoundationDBGlobalLock :: MVar ()
startFoundationDBGlobalLock = unsafePerformIO newEmptyMVar
{-# NOINLINE startFoundationDBGlobalLock #-}

-- | Starts up FoundationDB. You must call 'stopFoundationDB' before your
-- program terminates. It's recommended that you use 'withFoundationDB' instead,
-- since it handles cleanup. This function is only intended to be used in GHCi.
-- Can only be called once per process! Throws an 'Error' if any part of
-- setting up the connection FoundationDB fails.
startFoundationDB :: FoundationDBOptions
                  -> IO FDB.Database
startFoundationDB FoundationDBOptions{..} = do
  validateVersion apiVersion
  fdbThrowing' $ FDB.selectAPIVersion apiVersion
  forM_ networkOptions (fdbThrowing' . FDB.networkSetOption)
  fdbThrowing' FDB.setupNetwork
  void $ forkFinally FDB.runNetwork
                     (\_ -> putMVar startFoundationDBGlobalLock ())
#if FDB_API_VERSION < 610
  cluster <- initCluster (fromMaybe "" clusterFile)
  db <- initDB cluster
#else
  db <- fdbThrowing $ FDB.createDatabase (fromMaybe "" clusterFile)
#endif
  forM_ databaseOptions (fdbThrowing' . FDB.databaseSetOption db)
  return db

-- | Stops FoundationDB. For use with 'startFoundationDB'.
stopFoundationDB :: IO ()
stopFoundationDB = FDB.stopNetwork >> takeMVar startFoundationDBGlobalLock
