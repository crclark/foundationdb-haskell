{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | NOTE: This file is generated from <https://github.com/apple/foundationdb/blob/master/fdbclient/vexillographer/fdb.options fdb.options>
-- by the generate-options executable in this project.
-- All documentation on the individual options in this namespace comes
-- from FoundationDB's documentation in @fdb.options@.
module FoundationDB.Options where
import Data.ByteString.Char8 (ByteString)

data NetworkOption = NetworkOptionString Int String
                   | NetworkOptionInt Int Int
                   | NetworkOptionBytes Int ByteString
                   | NetworkOptionFlag Int
                       deriving (Show, Read, Eq, Ord)

-- | Deprecated
localAddress str = NetworkOptionString (10) str

-- | Deprecated
clusterFile str = NetworkOptionString (20) str

-- | Enables trace output to a file in a directory of the clients choosing
traceEnable str = NetworkOptionString (30) str

-- | Sets the maximum size in bytes of a single trace output file. This value should be in the range ``[0, INT64_MAX]``. If the value is set to 0, there is no limit on individual file size. The default is a maximum size of 10,485,760 bytes.
traceRollSize i = NetworkOptionInt (31) i

-- | Sets the maximum size of all the trace output files put together. This value should be in the range ``[0, INT64_MAX]``. If the value is set to 0, there is no limit on the total size of the files. The default is a maximum size of 104,857,600 bytes. If the default roll size is used, this means that a maximum of 10 trace files will be written at a time.
traceMaxLogsSize i = NetworkOptionInt (32) i

-- | Sets the 'logGroup' attribute with the specified value for all events in the trace output files. The default log group is 'default'.
traceLogGroup str = NetworkOptionString (33) str

-- | Set internal tuning or debugging knobs
knob str = NetworkOptionString (40) str

-- | Set the TLS plugin to load. This option, if used, must be set before any other TLS options
tlsPlugin str = NetworkOptionString (41) str

-- | Set the certificate chain
tlsCertBytes bs = NetworkOptionBytes (42) bs

-- | Set the file from which to load the certificate chain
tlsCertPath str = NetworkOptionString (43) str

-- | Set the private key corresponding to your own certificate
tlsKeyBytes bs = NetworkOptionBytes (45) bs

-- | Set the file from which to load the private key corresponding to your own certificate
tlsKeyPath str = NetworkOptionString (46) str

-- | Set the peer certificate field verification criteria
tlsVerifyPeers bs = NetworkOptionBytes (47) bs

-- |
buggifyEnable = NetworkOptionFlag (48)

-- |
buggifyDisable = NetworkOptionFlag (49)

-- | Set the probability of a BUGGIFY section being active for the current execution.  Only applies to code paths first traversed AFTER this option is changed.
buggifySectionActivatedProbability i = NetworkOptionInt (50) i

-- | Set the probability of an active BUGGIFY section being fired
buggifySectionFiredProbability i = NetworkOptionInt (51) i

-- | Disables the multi-version client API and instead uses the local client directly. Must be set before setting up the network.
disableMultiVersionClientApi = NetworkOptionFlag (60)

-- | If set, callbacks from external client libraries can be called from threads created by the FoundationDB client library. Otherwise, callbacks will be called from either the thread used to add the callback or the network thread. Setting this option can improve performance when connected using an external client, but may not be safe to use in all environments. Must be set before setting up the network. WARNING: This feature is considered experimental at this time.
callbacksOnExternalThreads = NetworkOptionFlag (61)

-- | Adds an external client library for use by the multi-version client API. Must be set before setting up the network.
externalClientLibrary str = NetworkOptionString (62) str

-- | Searches the specified path for dynamic libraries and adds them to the list of client libraries for use by the multi-version client API. Must be set before setting up the network.
externalClientDirectory str = NetworkOptionString (63) str

-- | Prevents connections through the local client, allowing only connections through externally loaded client libraries. Intended primarily for testing.
disableLocalClient = NetworkOptionFlag (64)

-- | Disables logging of client statistics, such as sampled transaction activity.
disableClientStatisticsLogging = NetworkOptionFlag (70)

-- | Enables debugging feature to perform slow task profiling. Requires trace logging to be enabled. WARNING: this feature is not recommended for use in production.
enableSlowTaskProfiling = NetworkOptionFlag (71)

-- | This option is set automatically to communicate the list of supported clients to the active client.
supportedClientVersions str = NetworkOptionString (1000) str

-- | This option is set automatically on all clients loaded externally using the multi-version API.
externalClient = NetworkOptionFlag (1001)

-- | This option tells a child on a multiversion client what transport ID to use.
externalClientTransportId i = NetworkOptionInt (1002) i


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


data TransactionOption = TransactionOptionString Int String
                       | TransactionOptionInt Int Int
                       | TransactionOptionBytes Int ByteString
                       | TransactionOptionFlag Int
                           deriving (Show, Read, Eq, Ord)

-- | The transaction, if not self-conflicting, may be committed a second time after commit succeeds, in the event of a fault
causalWriteRisky = TransactionOptionFlag (10)

-- | The read version will be committed, and usually will be the latest committed, but might not be the latest committed in the event of a fault or partition
causalReadRisky = TransactionOptionFlag (20)

-- |
causalReadDisable = TransactionOptionFlag (21)

-- | The next write performed on this transaction will not generate a write conflict range. As a result, other transactions which read the key(s) being modified by the next write will not conflict with this transaction. Care needs to be taken when using this option on a transaction that is shared between multiple threads. When setting this option, write conflict ranges will be disabled on the next write operation, regardless of what thread it is on.
nextWriteNoWriteConflictRange = TransactionOptionFlag (30)

-- | Committing this transaction will bypass the normal load balancing across proxies and go directly to the specifically nominated 'first proxy'.
commitOnFirstProxy = TransactionOptionFlag (40)

-- |
checkWritesEnable = TransactionOptionFlag (50)

-- | Reads performed by a transaction will not see any prior mutations that occured in that transaction, instead seeing the value which was in the database at the transaction's read version. This option may provide a small performance benefit for the client, but also disables a number of client-side optimizations which are beneficial for transactions which tend to read and write the same keys within a single transaction.
readYourWritesDisable = TransactionOptionFlag (51)

-- | Disables read-ahead caching for range reads. Under normal operation, a transaction will read extra rows from the database into cache if range reads are used to page through a series of data one row at a time (i.e. if a range read with a one row limit is followed by another one row range read starting immediately after the result of the first).
readAheadDisable = TransactionOptionFlag (52)

-- |
durabilityDatacenter = TransactionOptionFlag (110)

-- |
durabilityRisky = TransactionOptionFlag (120)

-- |
durabilityDevNullIsWebScale = TransactionOptionFlag (130)

-- | Specifies that this transaction should be treated as highest priority and that lower priority transactions should block behind this one. Use is discouraged outside of low-level tools
prioritySystemImmediate = TransactionOptionFlag (200)

-- | Specifies that this transaction should be treated as low priority and that default priority transactions should be processed first. Useful for doing batch work simultaneously with latency-sensitive work
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

-- | Enables tracing for this transaction and logs results to the client trace logs. Client trace logging must be enabled to get log output.
transactionLoggingEnable str = TransactionOptionString (402) str

-- | Set a timeout in milliseconds which, when elapsed, will cause the transaction automatically to be cancelled. Valid parameter values are ``[0, INT_MAX]``. If set to 0, will disable all timeouts. All pending and any future uses of the transaction will throw an exception. The transaction can be used again after it is reset. Like all transaction options, a timeout must be reset after a call to onError. This behavior allows the user to make the timeout dynamic.
timeout i = TransactionOptionInt (500) i

-- | Set a maximum number of retries after which additional calls to onError will throw the most recently seen error code. Valid parameter values are ``[-1, INT_MAX]``. If set to -1, will disable the retry limit. Like all transaction options, the retry limit must be reset after a call to onError. This behavior allows the user to make the retry limit dynamic.
retryLimit i = TransactionOptionInt (501) i

-- | Set the maximum amount of backoff delay incurred in the call to onError if the error is retryable. Defaults to 1000 ms. Valid parameter values are ``[0, INT_MAX]``. Like all transaction options, the maximum retry delay must be reset after a call to onError. If the maximum retry delay is less than the current retry delay of the transaction, then the current retry delay will be clamped to the maximum retry delay.
maxRetryDelay i = TransactionOptionInt (502) i

-- | Snapshot read operations will see the results of writes done in the same transaction.
snapshotRywEnable = TransactionOptionFlag (600)

-- | Snapshot read operations will not see the results of writes done in the same transaction.
snapshotRywDisable = TransactionOptionFlag (601)

-- | The transaction can read and write to locked databases, and is resposible for checking that it took the lock.
lockAware = TransactionOptionFlag (700)

-- | By default, operations that are performed on a transaction while it is being committed will not only fail themselves, but they will attempt to fail other in-flight operations (such as the commit) as well. This behavior is intended to help developers discover situations where operations could be unintentionally executed after the transaction has been reset. Setting this option removes that protection, causing only the offending operation to fail.
usedDuringCommitProtectionDisable = TransactionOptionFlag (701)

-- | The transaction can read from locked databases.
readLockAware = TransactionOptionFlag (702)


data StreamingMode = StreamingModeString Int String
                   | StreamingModeInt Int Int
                   | StreamingModeBytes Int ByteString
                   | StreamingModeFlag Int
                       deriving (Show, Read, Eq, Ord)

-- | Client intends to consume the entire range and would like it all transferred as early as possible.
wantAll = StreamingModeFlag (-2)

-- | The default. The client doesn't know how much of the range it is likely to used and wants different performance concerns to be balanced. Only a small portion of data is transferred to the client initially (in order to minimize costs if the client doesn't read the entire range), and as the caller iterates over more items in the range larger batches will be transferred in order to minimize latency.
iterator = StreamingModeFlag (-1)

-- | Infrequently used. The client has passed a specific row limit and wants that many rows delivered in a single batch. Because of iterator operation in client drivers make request batches transparent to the user, consider ``WANT_ALL`` StreamingMode instead. A row limit must be specified if this mode is used.
exact = StreamingModeFlag (0)

-- | Infrequently used. Transfer data in batches small enough to not be much more expensive than reading individual rows, to minimize cost if iteration stops early.
small = StreamingModeFlag (1)

-- | Infrequently used. Transfer data in batches sized in between small and large.
medium = StreamingModeFlag (2)

-- | Infrequently used. Transfer data in batches large enough to be, in a high-concurrency environment, nearly as efficient as possible. If the client stops iteration early, some disk and network bandwidth may be wasted. The batch size may still be too small to allow a single client to get high throughput from the database, so if that is what you need consider the SERIAL StreamingMode.
large = StreamingModeFlag (3)

-- | Transfer data in batches large enough that an individual client can get reasonable read bandwidth from the database. If the client stops iteration early, considerable disk and network bandwidth may be wasted.
serial = StreamingModeFlag (4)


data MutationType = MutationTypeString Int String
                  | MutationTypeInt Int Int
                  | MutationTypeBytes Int ByteString
                  | MutationTypeFlag Int
                      deriving (Show, Read, Eq, Ord)

-- | Performs an addition of little-endian integers. If the existing value in the database is not present or shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``. The integers to be added must be stored in a little-endian representation.  They can be signed in two's complement representation or unsigned. You can add to an integer at a known offset in the value by prepending the appropriate number of zero bytes to ``param`` and padding with zero bytes to match the length of the value. However, this offset technique requires that you know the addition will not cause the integer field within the value to overflow.
add bs = MutationTypeBytes (2) bs

-- | Deprecated
and bs = MutationTypeBytes (6) bs

-- | Performs a bitwise ``and`` operation.  If the existing value in the database is not present, then ``param`` is stored in the database. If the existing value in the database is shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``.
bitAnd bs = MutationTypeBytes (6) bs

-- | Deprecated
or bs = MutationTypeBytes (7) bs

-- | Performs a bitwise ``or`` operation.  If the existing value in the database is not present or shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``.
bitOr bs = MutationTypeBytes (7) bs

-- | Deprecated
xor bs = MutationTypeBytes (8) bs

-- | Performs a bitwise ``xor`` operation.  If the existing value in the database is not present or shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``.
bitXor bs = MutationTypeBytes (8) bs

-- | Performs a little-endian comparison of byte strings. If the existing value in the database is not present or shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``. The larger of the two values is then stored in the database.
max bs = MutationTypeBytes (12) bs

-- | Performs a little-endian comparison of byte strings. If the existing value in the database is not present, then ``param`` is stored in the database. If the existing value in the database is shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``. The smaller of the two values is then stored in the database.
min bs = MutationTypeBytes (13) bs

-- | Transforms ``key`` using a versionstamp for the transaction. Sets the transformed key in the database to ``param``. A versionstamp is a 10 byte, unique, monotonically (but not sequentially) increasing value for each committed transaction. The first 8 bytes are the committed version of the database. The last 2 bytes are monotonic in the serialization order for transactions. WARNING: At this time versionstamps are compatible with the Tuple layer only in the Java and Python bindings. Note that this implies versionstamped keys may not be used with the Subspace and Directory layers except in those languages.
setVersionstampedKey bs = MutationTypeBytes (14) bs

-- | Transforms ``param`` using a versionstamp for the transaction. Sets ``key`` in the database to the transformed parameter. A versionstamp is a 10 byte, unique, monotonically (but not sequentially) increasing value for each committed transaction. The first 8 bytes are the committed version of the database. The last 2 bytes are monotonic in the serialization order for transactions. WARNING: At this time versionstamped values are not compatible with the Tuple layer.
setVersionstampedValue bs = MutationTypeBytes (15) bs

-- | Performs lexicographic comparison of byte strings. If the existing value in the database is not present, then ``param`` is stored. Otherwise the smaller of the two values is then stored in the database.
byteMin bs = MutationTypeBytes (16) bs

-- | Performs lexicographic comparison of byte strings. If the existing value in the database is not present, then ``param`` is stored. Otherwise the larger of the two values is then stored in the database.
byteMax bs = MutationTypeBytes (17) bs


data ConflictRangeType = ConflictRangeTypeString Int String
                       | ConflictRangeTypeInt Int Int
                       | ConflictRangeTypeBytes Int ByteString
                       | ConflictRangeTypeFlag Int
                           deriving (Show, Read, Eq, Ord)

-- | Used to add a read conflict range
read = ConflictRangeTypeFlag (0)

-- | Used to add a write conflict range
write = ConflictRangeTypeFlag (1)


data ErrorPredicate = ErrorPredicateString Int String
                    | ErrorPredicateInt Int Int
                    | ErrorPredicateBytes Int ByteString
                    | ErrorPredicateFlag Int
                        deriving (Show, Read, Eq, Ord)

-- | Returns ``true`` if the error indicates the operations in the transactions should be retried because of transient error.
retryable = ErrorPredicateFlag (50000)

-- | Returns ``true`` if the error indicates the transaction may have succeeded, though not in a way the system can verify.
maybeCommitted = ErrorPredicateFlag (50001)

-- | Returns ``true`` if the error indicates the transaction has not committed, though in a way that can be retried.
retryableNotCommitted = ErrorPredicateFlag (50002)



