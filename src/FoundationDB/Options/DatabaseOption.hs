{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | NOTE: This file is generated from <https://github.com/apple/foundationdb/blob/master/fdbclient/vexillographer/fdb.options fdb.options>
-- by the generate-options executable in this project.
-- All documentation on the individual options in this namespace comes
-- from FoundationDB's documentation in @fdb.options@.
module FoundationDB.Options.DatabaseOption where
import Data.ByteString.Char8 (ByteString)

data DatabaseOption = DatabaseOptionString Int String
                    | DatabaseOptionInt Int Int
                    | DatabaseOptionBytes Int ByteString
                    | DatabaseOptionFlag Int
                        deriving (Show, Read, Eq, Ord)

-- | Set the size of the client location cache. Raising this value can boost performance in very large databases where clients access data in a near-random pattern. Defaults to 100000.
locationCacheSize i = DatabaseOptionInt (10) i

-- | Set the maximum number of watches allowed to be outstanding on a database connection. Increasing this number could result in increased resource usage. Reducing this number will not cancel any outstanding watches. Defaults to 10000 and cannot be larger than 1000000.
maxWatches i = DatabaseOptionInt (20) i

-- | Specify the machine ID that was passed to fdbserver processes running on the same machine as this client, for better location-aware load balancing.
machineId str = DatabaseOptionString (21) str

-- | Specify the datacenter ID that was passed to fdbserver processes running in the same datacenter as this client, for better location-aware load balancing.
datacenterId str = DatabaseOptionString (22) str

-- | Snapshot read operations will see the results of writes done in the same transaction. This is the default behavior.
snapshotRywEnable = DatabaseOptionFlag (26)

-- | Snapshot read operations will not see the results of writes done in the same transaction. This was the default behavior prior to API version 300.
snapshotRywDisable = DatabaseOptionFlag (27)

-- | Sets the maximum escaped length of key and value fields to be logged to the trace file via the LOG_TRANSACTION option. This sets the ``transaction_logging_max_field_length`` option of each transaction created by this database. See the transaction option description for more information.
transactionLoggingMaxFieldLength i = DatabaseOptionInt (405) i

-- | Set a timeout in milliseconds which, when elapsed, will cause each transaction automatically to be cancelled. This sets the ``timeout`` option of each transaction created by this database. See the transaction option description for more information. Using this option requires that the API version is 610 or higher.
transactionTimeout i = DatabaseOptionInt (500) i

-- | Set a maximum number of retries after which additional calls to ``onError`` will throw the most recently seen error code. This sets the ``retry_limit`` option of each transaction created by this database. See the transaction option description for more information.
transactionRetryLimit i = DatabaseOptionInt (501) i

-- | Set the maximum amount of backoff delay incurred in the call to ``onError`` if the error is retryable. This sets the ``max_retry_delay`` option of each transaction created by this database. See the transaction option description for more information.
transactionMaxRetryDelay i = DatabaseOptionInt (502) i

-- | Set the maximum transaction size in bytes. This sets the ``size_limit`` option on each transaction created by this database. See the transaction option description for more information.
transactionSizeLimit i = DatabaseOptionInt (503) i

-- | The read version will be committed, and usually will be the latest committed, but might not be the latest committed in the event of a simultaneous fault and misbehaving clock.
transactionCausalReadRisky = DatabaseOptionFlag (504)

-- | Deprecated. Addresses returned by get_addresses_for_key include the port when enabled. As of api version 630, this option is enabled by default and setting this has no effect.
transactionIncludePortInAddress = DatabaseOptionFlag (505)

-- | Allows ``get`` operations to read from sections of keyspace that have become unreadable because of versionstamp operations. This sets the ``bypass_unreadable`` option of each transaction created by this database. See the transaction option description for more information.
transactionBypassUnreadable = DatabaseOptionFlag (700)

-- | Use configuration database.
useConfigDatabase = DatabaseOptionFlag (800)

-- | An integer between 0 and 100 (default is 0) expressing the probability that a client will verify it can't read stale data whenever it detects a recovery.
testCausalReadRisky = DatabaseOptionFlag (900)

