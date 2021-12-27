module FoundationDB.Internal.Database (
  FoundationDBOptions(..),
  defaultOptions,
  Database(..),
  apiVersionInUse
) where

import qualified FoundationDB.Internal.Bindings as FDB
import FoundationDB.Options.DatabaseOption (DatabaseOption(..))
import FoundationDB.Options.NetworkOption (NetworkOption(..))

-- | Options set at the connection level for FoundationDB.
data FoundationDBOptions = FoundationDBOptions
  { apiVersion :: Int
    -- ^ Desired API version. See 'currentAPIVersion' for the latest
    -- version installed on your system. The C API (and this library) allow you
    -- to choose any version earlier than 'currentAPIVersion' to get the client
    -- behavior of that version of the FoundationDB client library.
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

data Database = Database
  { databasePtr :: FDB.DatabasePtr
  , databaseFoundationDBOptions :: FoundationDBOptions
  } deriving (Show, Eq)

-- | Returns the API version that was specified in the 'apiVersion' field when
-- the FDB client was initialized.
apiVersionInUse :: Database -> Int
apiVersionInUse = apiVersion . databaseFoundationDBOptions
