{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- NOTE: This file is generated from fdb.options
-- https://github.com/apple/foundationdb/blob/master/fdbclient/vexillographer/fdb.options
-- by the generate-options executable in this project.

module FoundationDB.Options where

import Data.ByteString.Char8 (ByteString)

data NetworkOption = NetworkOptionStringOption Int String
                   | NetworkOptionIntOption Int Int
                   | NetworkOptionBytesOption Int ByteString
                   | NetworkOptionFlagOption Int

-- | Deprecated
localAddress str = NetworkOptionStringOption (10) str

-- | Deprecated
clusterFile str = NetworkOptionStringOption (20) str

-- | Enables trace output to a file in a directory of the clients choosing
traceEnable str = NetworkOptionStringOption (30) str

-- | Sets the maximum size in bytes of a single trace output file. This value should be in the range ``[0, INT64_MAX]``. If the value is set to 0, there is no limit on individual file size. The default is a maximum size of 10,485,760 bytes.
traceRollSize i = NetworkOptionIntOption (31) i

-- | Sets the maximum size of all the trace output files put together. This value should be in the range ``[0, INT64_MAX]``. If the value is set to 0, there is no limit on the total size of the files. The default is a maximum size of 104,857,600 bytes. If the default roll size is used, this means that a maximum of 10 trace files will be written at a time.
traceMaxLogsSize i = NetworkOptionIntOption (32) i

-- | Sets the 'logGroup' attribute with the specified value for all events in the trace output files. The default log group is 'default'.
traceLogGroup str = NetworkOptionStringOption (33) str

-- | Set internal tuning or debugging knobs
knob str = NetworkOptionStringOption (40) str

-- | Set the TLS plugin to load. This option, if used, must be set before any other TLS options
tlsPlugin str = NetworkOptionStringOption (41) str

-- | Set the certificate chain
tlsCertBytes bs = NetworkOptionBytesOption (42) bs

-- | Set the file from which to load the certificate chain
tlsCertPath str = NetworkOptionStringOption (43) str

-- | Set the private key corresponding to your own certificate
tlsKeyBytes bs = NetworkOptionBytesOption (45) bs

-- | Set the file from which to load the private key corresponding to your own certificate
tlsKeyPath str = NetworkOptionStringOption (46) str

-- | Set the peer certificate field verification criteria
tlsVerifyPeers bs = NetworkOptionBytesOption (47) bs

-- | 
buggifyEnable = NetworkOptionFlagOption (48)

-- | 
buggifyDisable = NetworkOptionFlagOption (49)

-- | Set the probability of a BUGGIFY section being active for the current execution.  Only applies to code paths first traversed AFTER this option is changed.
buggifySectionActivatedProbability i
  = NetworkOptionIntOption (50) i

-- | Set the probability of an active BUGGIFY section being fired
buggifySectionFiredProbability i = NetworkOptionIntOption (51) i

-- | Disables the multi-version client API and instead uses the local client directly. Must be set before setting up the network.
disableMultiVersionClientApi = NetworkOptionFlagOption (60)

-- | If set, callbacks from external client libraries can be called from threads created by the FoundationDB client library. Otherwise, callbacks will be called from either the thread used to add the callback or the network thread. Setting this option can improve performance when connected using an external client, but may not be safe to use in all environments. Must be set before setting up the network. WARNING: This feature is considered experimental at this time.
callbacksOnExternalThreads = NetworkOptionFlagOption (61)

-- | Adds an external client library for use by the multi-version client API. Must be set before setting up the network.
externalClientLibrary str = NetworkOptionStringOption (62) str

-- | Searches the specified path for dynamic libraries and adds them to the list of client libraries for use by the multi-version client API. Must be set before setting up the network.
externalClientDirectory str = NetworkOptionStringOption (63) str

-- | Prevents connections through the local client, allowing only connections through externally loaded client libraries. Intended primarily for testing.
disableLocalClient = NetworkOptionFlagOption (64)

-- | Disables logging of client statistics, such as sampled transaction activity.
disableClientStatisticsLogging = NetworkOptionFlagOption (70)

-- | Enables debugging feature to perform slow task profiling. Requires trace logging to be enabled. WARNING: this feature is not recommended for use in production.
enableSlowTaskProfiling = NetworkOptionFlagOption (71)

-- | This option is set automatically to communicate the list of supported clients to the active client.
supportedClientVersions str = NetworkOptionStringOption (1000) str

-- | This option is set automatically on all clients loaded externally using the multi-version API.
externalClient = NetworkOptionFlagOption (1001)

-- | This option tells a child on a multiversion client what transport ID to use.
externalClientTransportId i = NetworkOptionIntOption (1002) i


data DatabaseOption = DatabaseOptionStringOption Int String
                    | DatabaseOptionIntOption Int Int
                    | DatabaseOptionBytesOption Int ByteString
                    | DatabaseOptionFlagOption Int

-- | Set the size of the client location cache. Raising this value can boost performance in very large databases where clients access data in a near-random pattern. Defaults to 100000.
locationCacheSize i = DatabaseOptionIntOption (10) i

-- | Set the maximum number of watches allowed to be outstanding on a database connection. Increasing this number could result in increased resource usage. Reducing this number will not cancel any outstanding watches. Defaults to 10000 and cannot be larger than 1000000.
maxWatches i = DatabaseOptionIntOption (20) i

-- | Specify the machine ID that was passed to fdbserver processes running on the same machine as this client, for better location-aware load balancing.
machineId str = DatabaseOptionStringOption (21) str

-- | Specify the datacenter ID that was passed to fdbserver processes running in the same datacenter as this client, for better location-aware load balancing.
datacenterId str = DatabaseOptionStringOption (22) str


data TransactionOption = TransactionOptionStringOption Int String
                       | TransactionOptionIntOption Int Int
                       | TransactionOptionBytesOption Int ByteString
                       | TransactionOptionFlagOption Int

-- | The transaction, if not self-conflicting, may be committed a second time after commit succeeds, in the event of a fault
causalWriteRisky = TransactionOptionFlagOption (10)

-- | The read version will be committed, and usually will be the latest committed, but might not be the latest committed in the event of a fault or partition
causalReadRisky = TransactionOptionFlagOption (20)

-- | 
causalReadDisable = TransactionOptionFlagOption (21)

-- | The next write performed on this transaction will not generate a write conflict range. As a result, other transactions which read the key(s) being modified by the next write will not conflict with this transaction. Care needs to be taken when using this option on a transaction that is shared between multiple threads. When setting this option, write conflict ranges will be disabled on the next write operation, regardless of what thread it is on.
nextWriteNoWriteConflictRange = TransactionOptionFlagOption (30)

-- | Committing this transaction will bypass the normal load balancing across proxies and go directly to the specifically nominated 'first proxy'.
commitOnFirstProxy = TransactionOptionFlagOption (40)

-- | 
checkWritesEnable = TransactionOptionFlagOption (50)

-- | Reads performed by a transaction will not see any prior mutations that occured in that transaction, instead seeing the value which was in the database at the transaction's read version. This option may provide a small performance benefit for the client, but also disables a number of client-side optimizations which are beneficial for transactions which tend to read and write the same keys within a single transaction.
readYourWritesDisable = TransactionOptionFlagOption (51)

-- | Deprecated
readAheadDisable = TransactionOptionFlagOption (52)

-- | 
durabilityDatacenter = TransactionOptionFlagOption (110)

-- | 
durabilityRisky = TransactionOptionFlagOption (120)

-- | Specifies that this transaction should be treated as highest priority and that lower priority transactions should block behind this one. Use is discouraged outside of low-level tools
prioritySystemImmediate = TransactionOptionFlagOption (200)

-- | Specifies that this transaction should be treated as low priority and that default priority transactions should be processed first. Useful for doing batch work simultaneously with latency-sensitive work
priorityBatch = TransactionOptionFlagOption (201)

-- | This is a write-only transaction which sets the initial configuration. This option is designed for use by database system tools only.
initializeNewDatabase = TransactionOptionFlagOption (300)

-- | Allows this transaction to read and modify system keys (those that start with the byte 0xFF)
accessSystemKeys = TransactionOptionFlagOption (301)

-- | Allows this transaction to read system keys (those that start with the byte 0xFF)
readSystemKeys = TransactionOptionFlagOption (302)

-- | 
debugDump = TransactionOptionFlagOption (400)

-- | 
debugRetryLogging str = TransactionOptionStringOption (401) str

-- | Enables tracing for this transaction and logs results to the client trace logs. Client trace logging must be enabled to get log output.
transactionLoggingEnable str
  = TransactionOptionStringOption (402) str

-- | Set a timeout in milliseconds which, when elapsed, will cause the transaction automatically to be cancelled. Valid parameter values are ``[0, INT_MAX]``. If set to 0, will disable all timeouts. All pending and any future uses of the transaction will throw an exception. The transaction can be used again after it is reset. Like all transaction options, a timeout must be reset after a call to onError. This behavior allows the user to make the timeout dynamic.
timeout i = TransactionOptionIntOption (500) i

-- | Set a maximum number of retries after which additional calls to onError will throw the most recently seen error code. Valid parameter values are ``[-1, INT_MAX]``. If set to -1, will disable the retry limit. Like all transaction options, the retry limit must be reset after a call to onError. This behavior allows the user to make the retry limit dynamic.
retryLimit i = TransactionOptionIntOption (501) i

-- | Set the maximum amount of backoff delay incurred in the call to onError if the error is retryable. Defaults to 1000 ms. Valid parameter values are ``[0, INT_MAX]``. Like all transaction options, the maximum retry delay must be reset after a call to onError. If the maximum retry delay is less than the current retry delay of the transaction, then the current retry delay will be clamped to the maximum retry delay.
maxRetryDelay i = TransactionOptionIntOption (502) i

-- | Snapshot read operations will see the results of writes done in the same transaction.
snapshotRywEnable = TransactionOptionFlagOption (600)

-- | Snapshot read operations will not see the results of writes done in the same transaction.
snapshotRywDisable = TransactionOptionFlagOption (601)

-- | The transaction can read and write to locked databases, and is resposible for checking that it took the lock.
lockAware = TransactionOptionFlagOption (700)

-- | By default, operations that are performed on a transaction while it is being committed will not only fail themselves, but they will attempt to fail other in-flight operations (such as the commit) as well. This behavior is intended to help developers discover situations where operations could be unintentionally executed after the transaction has been reset. Setting this option removes that protection, causing only the offending operation to fail.
usedDuringCommitProtectionDisable
  = TransactionOptionFlagOption (701)

-- | The transaction can read from locked databases.
readLockAware = TransactionOptionFlagOption (702)

-- | No other transactions will be applied before this transaction within the same commit version.
firstInBatch = TransactionOptionFlagOption (710)


data StreamingMode = StreamingModeStringOption Int String
                   | StreamingModeIntOption Int Int
                   | StreamingModeBytesOption Int ByteString
                   | StreamingModeFlagOption Int

-- | Client intends to consume the entire range and would like it all transferred as early as possible.
wantAll = StreamingModeFlagOption (-2)

-- | The default. The client doesn't know how much of the range it is likely to used and wants different performance concerns to be balanced. Only a small portion of data is transferred to the client initially (in order to minimize costs if the client doesn't read the entire range), and as the caller iterates over more items in the range larger batches will be transferred in order to minimize latency.
iterator = StreamingModeFlagOption (-1)

-- | Infrequently used. The client has passed a specific row limit and wants that many rows delivered in a single batch. Because of iterator operation in client drivers make request batches transparent to the user, consider ``WANT_ALL`` StreamingMode instead. A row limit must be specified if this mode is used.
exact = StreamingModeFlagOption (0)

-- | Infrequently used. Transfer data in batches small enough to not be much more expensive than reading individual rows, to minimize cost if iteration stops early.
small = StreamingModeFlagOption (1)

-- | Infrequently used. Transfer data in batches sized in between small and large.
medium = StreamingModeFlagOption (2)

-- | Infrequently used. Transfer data in batches large enough to be, in a high-concurrency environment, nearly as efficient as possible. If the client stops iteration early, some disk and network bandwidth may be wasted. The batch size may still be too small to allow a single client to get high throughput from the database, so if that is what you need consider the SERIAL StreamingMode.
large = StreamingModeFlagOption (3)

-- | Transfer data in batches large enough that an individual client can get reasonable read bandwidth from the database. If the client stops iteration early, considerable disk and network bandwidth may be wasted.
serial = StreamingModeFlagOption (4)


data MutationType = MutationTypeStringOption Int String
                  | MutationTypeIntOption Int Int
                  | MutationTypeBytesOption Int ByteString
                  | MutationTypeFlagOption Int

-- | Performs an addition of little-endian integers. If the existing value in the database is not present or shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``. The integers to be added must be stored in a little-endian representation.  They can be signed in two's complement representation or unsigned. You can add to an integer at a known offset in the value by prepending the appropriate number of zero bytes to ``param`` and padding with zero bytes to match the length of the value. However, this offset technique requires that you know the addition will not cause the integer field within the value to overflow.
add bs = MutationTypeBytesOption (2) bs

-- | Deprecated
and bs = MutationTypeBytesOption (6) bs

-- | Performs a bitwise ``and`` operation.  If the existing value in the database is not present, then ``param`` is stored in the database. If the existing value in the database is shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``.
bitAnd bs = MutationTypeBytesOption (6) bs

-- | Deprecated
or bs = MutationTypeBytesOption (7) bs

-- | Performs a bitwise ``or`` operation.  If the existing value in the database is not present or shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``.
bitOr bs = MutationTypeBytesOption (7) bs

-- | Deprecated
xor bs = MutationTypeBytesOption (8) bs

-- | Performs a bitwise ``xor`` operation.  If the existing value in the database is not present or shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``.
bitXor bs = MutationTypeBytesOption (8) bs

-- | Appends ``param`` to the end of the existing value already in the database at the given key (or creates the key and sets the value to ``param`` if the key is empty). This will only append the value if the final concatenated value size is less than or equal to the maximum value size (i.e., if it fits). WARNING: No error is surfaced back to the user if the final value is too large because the mutation will not be applied until after the transaction has been committed. Therefore, it is only safe to use this mutation type if one can guarantee that one will keep the total value size under the maximum size.
appendIfFits bs = MutationTypeBytesOption (9) bs

-- | Performs a little-endian comparison of byte strings. If the existing value in the database is not present or shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``. The larger of the two values is then stored in the database.
max bs = MutationTypeBytesOption (12) bs

-- | Performs a little-endian comparison of byte strings. If the existing value in the database is not present, then ``param`` is stored in the database. If the existing value in the database is shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``. The smaller of the two values is then stored in the database.
min bs = MutationTypeBytesOption (13) bs

-- | Transforms ``key`` using a versionstamp for the transaction. Sets the transformed key in the database to ``param``. A versionstamp is a 10 byte, unique, monotonically (but not sequentially) increasing value for each committed transaction. The first 8 bytes are the committed version of the database. The last 2 bytes are monotonic in the serialization order for transactions. WARNING: At this time versionstamps are compatible with the Tuple layer only in the Java and Python bindings. Note that this implies versionstamped keys may not be used with the Subspace and Directory layers except in those languages.
setVersionstampedKey bs = MutationTypeBytesOption (14) bs

-- | Transforms ``param`` using a versionstamp for the transaction. Sets ``key`` in the database to the transformed parameter. A versionstamp is a 10 byte, unique, monotonically (but not sequentially) increasing value for each committed transaction. The first 8 bytes are the committed version of the database. The last 2 bytes are monotonic in the serialization order for transactions. WARNING: At this time versionstamped values are not compatible with the Tuple layer.
setVersionstampedValue bs = MutationTypeBytesOption (15) bs

-- | Performs lexicographic comparison of byte strings. If the existing value in the database is not present, then ``param`` is stored. Otherwise the smaller of the two values is then stored in the database.
byteMin bs = MutationTypeBytesOption (16) bs

-- | Performs lexicographic comparison of byte strings. If the existing value in the database is not present, then ``param`` is stored. Otherwise the larger of the two values is then stored in the database.
byteMax bs = MutationTypeBytesOption (17) bs


data ConflictRangeType = ConflictRangeTypeStringOption Int String
                       | ConflictRangeTypeIntOption Int Int
                       | ConflictRangeTypeBytesOption Int ByteString
                       | ConflictRangeTypeFlagOption Int

-- | Used to add a read conflict range
read = ConflictRangeTypeFlagOption (0)

-- | Used to add a write conflict range
write = ConflictRangeTypeFlagOption (1)


data ErrorPredicate = ErrorPredicateStringOption Int String
                    | ErrorPredicateIntOption Int Int
                    | ErrorPredicateBytesOption Int ByteString
                    | ErrorPredicateFlagOption Int

-- | Returns ``true`` if the error indicates the operations in the transactions should be retried because of transient error.
retryable = ErrorPredicateFlagOption (50000)

-- | Returns ``true`` if the error indicates the transaction may have succeeded, though not in a way the system can verify.
maybeCommitted = ErrorPredicateFlagOption (50001)

-- | Returns ``true`` if the error indicates the transaction has not committed, though in a way that can be retried.
retryableNotCommitted = ErrorPredicateFlagOption (50002)



