{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
module FoundationDB
  ( -- * Initialization
    FDB.currentAPIVersion,
    withFoundationDB,
    FoundationDBOptions (..),
    defaultOptions,
    Database,
    apiVersionInUse,

    -- * Transactions
    module FoundationDB.Transaction,

    -- * Errors
    module FoundationDB.Error,

    -- * Helpers for ghci
    startFoundationDB,
    stopFoundationDB,
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import FoundationDB.Error
import FoundationDB.Error.Internal
import qualified FoundationDB.Internal.Bindings as FDB
import FoundationDB.Internal.Database
import FoundationDB.Transaction
import System.IO.Unsafe (unsafePerformIO)

-- | This library doesn't support FDB versions earlier than 5.2 (the first
-- open source release).
validateVersion :: Int -> IO ()
validateVersion v =
  when
    (v < 520)
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

initDB :: FDB.Cluster -> IO FDB.DatabasePtr
initDB cluster = do
  futureDB <- FDB.clusterCreateDatabase cluster
  fdbThrowing' $ FDB.futureBlockUntilReady futureDB
  fdbThrowing $ FDB.futureGetDatabase futureDB

withDatabase :: FoundationDBOptions -> (Database -> IO a) -> IO a
withDatabase opts@FoundationDBOptions{clusterFile} f =
  withCluster clusterFile $ \ cluster ->
    bracket (fmap (flip Database opts) $ initDB cluster)
            (FDB.databaseDestroy . databasePtr)
            f
#else
withDatabase :: FoundationDBOptions -> (Database -> IO a) -> IO a
withDatabase opts@FoundationDBOptions{clusterFile} =
  bracket (fmap (flip Database opts)
           $ fdbThrowing
           $ FDB.createDatabase (fromMaybe "" clusterFile))
          (FDB.databaseDestroy . databasePtr)
#endif

-- | Handles correctly starting up the network connection to the DB.
-- Can only be called once per process! Throws an 'Error' if any part of
-- setting up the connection to FoundationDB fails.
withFoundationDB ::
  FoundationDBOptions ->
  (Database -> IO a) ->
  IO a
withFoundationDB opts@FoundationDBOptions {..} m = do
  validateVersion apiVersion
  done <- newEmptyMVar
  fdbThrowing' $ FDB.selectAPIVersion apiVersion
  forM_ networkOptions (fdbThrowing' . FDB.networkSetOption)
  fdbThrowing' FDB.setupNetwork
  start done
  finally (withDatabase opts run) (stop done)
  where
    start done = void $ forkFinally FDB.runNetwork (\_ -> putMVar done ())
    stop done = FDB.stopNetwork >> takeMVar done
    run db@Database {databasePtr} = do
      forM_ databaseOptions (fdbThrowing' . FDB.databaseSetOption databasePtr)
      m db

startFoundationDBGlobalLock :: MVar ()
startFoundationDBGlobalLock = unsafePerformIO newEmptyMVar
{-# NOINLINE startFoundationDBGlobalLock #-}

-- | Starts up FoundationDB. You must call 'stopFoundationDB' before your
-- program terminates. It's recommended that you use 'withFoundationDB' instead,
-- since it handles cleanup. This function is only intended to be used in GHCi.
-- Can only be called once per process! Throws an 'Error' if any part of
-- setting up the connection FoundationDB fails.
startFoundationDB ::
  FoundationDBOptions ->
  IO Database
startFoundationDB opts@FoundationDBOptions {..} = do
  validateVersion apiVersion
  fdbThrowing' $ FDB.selectAPIVersion apiVersion
  forM_ networkOptions (fdbThrowing' . FDB.networkSetOption)
  fdbThrowing' FDB.setupNetwork
  void $
    forkFinally
      FDB.runNetwork
      (\_ -> putMVar startFoundationDBGlobalLock ())
#if FDB_API_VERSION < 610
  cluster <- initCluster (fromMaybe "" clusterFile)
  db <- initDB cluster
#else
  db <- fdbThrowing $ FDB.createDatabase (fromMaybe "" clusterFile)
#endif
  forM_ databaseOptions (fdbThrowing' . FDB.databaseSetOption db)
  return (Database {databasePtr = db, databaseFoundationDBOptions = opts})

-- | Stops FoundationDB. For use with 'startFoundationDB'.
stopFoundationDB :: IO ()
stopFoundationDB = FDB.stopNetwork >> takeMVar startFoundationDBGlobalLock
