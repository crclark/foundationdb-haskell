{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | NOTE: This file is generated from <https://github.com/apple/foundationdb/blob/master/fdbclient/vexillographer/fdb.options fdb.options>
-- by the generate-options executable in this project.
-- All documentation on the individual options in this namespace comes
-- from FoundationDB's documentation in @fdb.options@.
module FoundationDB.Options.TransactionOption where
import Data.ByteString.Char8 (ByteString)

{-# DEPRECATED
readAheadDisable "Deprecated in FDB C API"
 #-}
{-# DEPRECATED
durabilityDevNullIsWebScale "Deprecated in FDB C API"
 #-}
{-# DEPRECATED
transactionLoggingEnable "Deprecated in FDB C API"
 #-}
data TransactionOption = TransactionOptionString Int String
                       | TransactionOptionInt Int Int
                       | TransactionOptionBytes Int ByteString
                       | TransactionOptionFlag Int
                           deriving (Show, Read, Eq, Ord)

-- | The transaction, if not self-conflicting, may be committed a second time after commit succeeds, in the event of a fault
causalWriteRisky = TransactionOptionFlag (10)

-- | The read version will be committed, and usually will be the latest committed, but might not be the latest committed in the event of a simultaneous fault and misbehaving clock.
causalReadRisky = TransactionOptionFlag (20)

-- | 
causalReadDisable = TransactionOptionFlag (21)

-- | Addresses returned by get_addresses_for_key include the port when enabled. As of api version 630, this option is enabled by default and setting this has no effect.
includePortInAddress = TransactionOptionFlag (23)

-- | The next write performed on this transaction will not generate a write conflict range. As a result, other transactions which read the key(s) being modified by the next write will not conflict with this transaction. Care needs to be taken when using this option on a transaction that is shared between multiple threads. When setting this option, write conflict ranges will be disabled on the next write operation, regardless of what thread it is on.
nextWriteNoWriteConflictRange = TransactionOptionFlag (30)

-- | Committing this transaction will bypass the normal load balancing across proxies and go directly to the specifically nominated 'first proxy'.
commitOnFirstProxy = TransactionOptionFlag (40)

-- | 
checkWritesEnable = TransactionOptionFlag (50)

-- | Reads performed by a transaction will not see any prior mutations that occured in that transaction, instead seeing the value which was in the database at the transaction's read version. This option may provide a small performance benefit for the client, but also disables a number of client-side optimizations which are beneficial for transactions which tend to read and write the same keys within a single transaction.
readYourWritesDisable = TransactionOptionFlag (51)

-- | Deprecated
readAheadDisable = TransactionOptionFlag (52)

-- | 
durabilityDatacenter = TransactionOptionFlag (110)

-- | 
durabilityRisky = TransactionOptionFlag (120)

-- | Deprecated
durabilityDevNullIsWebScale = TransactionOptionFlag (130)

-- | Specifies that this transaction should be treated as highest priority and that lower priority transactions should block behind this one. Use is discouraged outside of low-level tools
prioritySystemImmediate = TransactionOptionFlag (200)

-- | Specifies that this transaction should be treated as low priority and that default priority transactions will be processed first. Batch priority transactions will also be throttled at load levels smaller than for other types of transactions and may be fully cut off in the event of machine failures. Useful for doing batch work simultaneously with latency-sensitive work
priorityBatch = TransactionOptionFlag (201)

-- | This is a write-only transaction which sets the initial configuration. This option is designed for use by database system tools only.
initializeNewDatabase = TransactionOptionFlag (300)

-- | Allows this transaction to read and modify system keys (those that start with the byte 0xFF)
accessSystemKeys = TransactionOptionFlag (301)

-- | Allows this transaction to read system keys (those that start with the byte 0xFF)
readSystemKeys = TransactionOptionFlag (302)

-- | 
debugDump = TransactionOptionFlag (400)

-- | 
debugRetryLogging str = TransactionOptionString (401) str

-- | Deprecated
transactionLoggingEnable str = TransactionOptionString (402) str

-- | Sets a client provided identifier for the transaction that will be used in scenarios like tracing or profiling. Client trace logging or transaction profiling must be separately enabled.
debugTransactionIdentifier str = TransactionOptionString (403) str

-- | Enables tracing for this transaction and logs results to the client trace logs. The DEBUG_TRANSACTION_IDENTIFIER option must be set before using this option, and client trace logging must be enabled to get log output.
logTransaction = TransactionOptionFlag (404)

-- | Sets the maximum escaped length of key and value fields to be logged to the trace file via the LOG_TRANSACTION option, after which the field will be truncated. A negative value disables truncation.
transactionLoggingMaxFieldLength i = TransactionOptionInt (405) i

-- | Sets an identifier for server tracing of this transaction. When committed, this identifier triggers logging when each part of the transaction authority encounters it, which is helpful in diagnosing slowness in misbehaving clusters. The identifier is randomly generated. When there is also a debug_transaction_identifier, both IDs are logged together.
serverRequestTracing = TransactionOptionFlag (406)

-- | Set a timeout in milliseconds which, when elapsed, will cause the transaction automatically to be cancelled. Valid parameter values are ``[0, INT_MAX]``. If set to 0, will disable all timeouts. All pending and any future uses of the transaction will throw an exception. The transaction can be used again after it is reset. Prior to API version 610, like all other transaction options, the timeout must be reset after a call to ``onError``. If the API version is 610 or greater, the timeout is not reset after an ``onError`` call. This allows the user to specify a longer timeout on specific transactions than the default timeout specified through the ``transaction_timeout`` database option without the shorter database timeout cancelling transactions that encounter a retryable error. Note that at all API versions, it is safe and legal to set the timeout each time the transaction begins, so most code written assuming the older behavior can be upgraded to the newer behavior without requiring any modification, and the caller is not required to implement special logic in retry loops to only conditionally set this option.
timeout i = TransactionOptionInt (500) i

-- | Set a maximum number of retries after which additional calls to ``onError`` will throw the most recently seen error code. Valid parameter values are ``[-1, INT_MAX]``. If set to -1, will disable the retry limit. Prior to API version 610, like all other transaction options, the retry limit must be reset after a call to ``onError``. If the API version is 610 or greater, the retry limit is not reset after an ``onError`` call. Note that at all API versions, it is safe and legal to set the retry limit each time the transaction begins, so most code written assuming the older behavior can be upgraded to the newer behavior without requiring any modification, and the caller is not required to implement special logic in retry loops to only conditionally set this option.
retryLimit i = TransactionOptionInt (501) i

-- | Set the maximum amount of backoff delay incurred in the call to ``onError`` if the error is retryable. Defaults to 1000 ms. Valid parameter values are ``[0, INT_MAX]``. If the maximum retry delay is less than the current retry delay of the transaction, then the current retry delay will be clamped to the maximum retry delay. Prior to API version 610, like all other transaction options, the maximum retry delay must be reset after a call to ``onError``. If the API version is 610 or greater, the retry limit is not reset after an ``onError`` call. Note that at all API versions, it is safe and legal to set the maximum retry delay each time the transaction begins, so most code written assuming the older behavior can be upgraded to the newer behavior without requiring any modification, and the caller is not required to implement special logic in retry loops to only conditionally set this option.
maxRetryDelay i = TransactionOptionInt (502) i

-- | Set the transaction size limit in bytes. The size is calculated by combining the sizes of all keys and values written or mutated, all key ranges cleared, and all read and write conflict ranges. (In other words, it includes the total size of all data included in the request to the cluster to commit the transaction.) Large transactions can cause performance problems on FoundationDB clusters, so setting this limit to a smaller value than the default can help prevent the client from accidentally degrading the cluster's performance. This value must be at least 32 and cannot be set to higher than 10,000,000, the default transaction size limit.
sizeLimit i = TransactionOptionInt (503) i

-- | Snapshot read operations will see the results of writes done in the same transaction. This is the default behavior.
snapshotRywEnable = TransactionOptionFlag (600)

-- | Snapshot read operations will not see the results of writes done in the same transaction. This was the default behavior prior to API version 300.
snapshotRywDisable = TransactionOptionFlag (601)

-- | The transaction can read and write to locked databases, and is responsible for checking that it took the lock.
lockAware = TransactionOptionFlag (700)

-- | By default, operations that are performed on a transaction while it is being committed will not only fail themselves, but they will attempt to fail other in-flight operations (such as the commit) as well. This behavior is intended to help developers discover situations where operations could be unintentionally executed after the transaction has been reset. Setting this option removes that protection, causing only the offending operation to fail.
usedDuringCommitProtectionDisable = TransactionOptionFlag (701)

-- | The transaction can read from locked databases.
readLockAware = TransactionOptionFlag (702)

-- | No other transactions will be applied before this transaction within the same commit version.
firstInBatch = TransactionOptionFlag (710)

-- | This option should only be used by tools which change the database configuration.
useProvisionalProxies = TransactionOptionFlag (711)

-- | The transaction can retrieve keys that are conflicting with other transactions.
reportConflictingKeys = TransactionOptionFlag (712)

-- | By default, the special key space will only allow users to read from exactly one module (a subspace in the special key space). Use this option to allow reading from zero or more modules. Users who set this option should be prepared for new modules, which may have different behaviors than the modules they're currently reading. For example, a new module might block or return an error.
specialKeySpaceRelaxed = TransactionOptionFlag (713)

-- | Adds a tag to the transaction that can be used to apply manual targeted throttling. At most 5 tags can be set on a transaction.
tag str = TransactionOptionString (800) str

-- | Adds a tag to the transaction that can be used to apply manual or automatic targeted throttling. At most 5 tags can be set on a transaction.
autoThrottleTag str = TransactionOptionString (801) str

