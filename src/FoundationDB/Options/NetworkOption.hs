{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | NOTE: This file is generated from <https://github.com/apple/foundationdb/blob/master/fdbclient/vexillographer/fdb.options fdb.options>
-- by the generate-options executable in this project.
-- All documentation on the individual options in this namespace comes
-- from FoundationDB's documentation in @fdb.options@.
module FoundationDB.Options.NetworkOption where
import Data.ByteString.Char8 (ByteString)

{-# DEPRECATED
localAddress "Deprecated in FDB C API"
 #-}
{-# DEPRECATED
clusterFile "Deprecated in FDB C API"
 #-}
{-# DEPRECATED
tlsPlugin "Deprecated in FDB C API"
 #-}
{-# DEPRECATED
enableSlowTaskProfiling "Deprecated in FDB C API"
 #-}
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

-- | Sets the 'LogGroup' attribute with the specified value for all events in the trace output files. The default log group is 'default'.
traceLogGroup str = NetworkOptionString (33) str

-- | Select the format of the log files. xml (the default) and json are supported.
traceFormat str = NetworkOptionString (34) str

-- | Select clock source for trace files. now (the default) or realtime are supported.
traceClockSource str = NetworkOptionString (35) str

-- | Once provided, this string will be used to replace the port/PID in the log file names.
traceFileIdentifier str = NetworkOptionString (36) str

-- | Set file suffix for partially written log files.
tracePartialFileSuffix str = NetworkOptionString (39) str

-- | Set internal tuning or debugging knobs
knob str = NetworkOptionString (40) str

-- | Deprecated
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

-- | Set the ca bundle
tlsCaBytes bs = NetworkOptionBytes (52) bs

-- | Set the file from which to load the certificate authority bundle
tlsCaPath str = NetworkOptionString (53) str

-- | Set the passphrase for encrypted private key. Password should be set before setting the key for the password to be used.
tlsPassword str = NetworkOptionString (54) str

-- | Disables the multi-version client API and instead uses the local client directly. Must be set before setting up the network.
disableMultiVersionClientApi = NetworkOptionFlag (60)

-- | If set, callbacks from external client libraries can be called from threads created by the FoundationDB client library. Otherwise, callbacks will be called from either the thread used to add the callback or the network thread. Setting this option can improve performance when connected using an external client, but may not be safe to use in all environments. Must be set before setting up the network. WARNING: This feature is considered experimental at this time.
callbacksOnExternalThreads = NetworkOptionFlag (61)

-- | Adds an external client library for use by the multi-version client API. Must be set before setting up the network.
externalClientLibrary str = NetworkOptionString (62) str

-- | Searches the specified path for dynamic libraries and adds them to the list of client libraries for use by the multi-version client API. Must be set before setting up the network.
externalClientDirectory str = NetworkOptionString (63) str

-- | Prevents connections through the local client, allowing only connections through externally loaded client libraries.
disableLocalClient = NetworkOptionFlag (64)

-- | Spawns multiple worker threads for each version of the client that is loaded.  Setting this to a number greater than one implies disable_local_client.
clientThreadsPerVersion i = NetworkOptionInt (65) i

-- | Disables logging of client statistics, such as sampled transaction activity.
disableClientStatisticsLogging = NetworkOptionFlag (70)

-- | Deprecated
enableSlowTaskProfiling = NetworkOptionFlag (71)

-- | Enables debugging feature to perform run loop profiling. Requires trace logging to be enabled. WARNING: this feature is not recommended for use in production.
enableRunLoopProfiling = NetworkOptionFlag (71)

-- | Enable client buggify - will make requests randomly fail (intended for client testing)
clientBuggifyEnable = NetworkOptionFlag (80)

-- | Disable client buggify
clientBuggifyDisable = NetworkOptionFlag (81)

-- | Set the probability of a CLIENT_BUGGIFY section being active for the current execution.
clientBuggifySectionActivatedProbability i
  = NetworkOptionInt (82) i

-- | Set the probability of an active CLIENT_BUGGIFY section being fired. A section will only fire if it was activated
clientBuggifySectionFiredProbability i = NetworkOptionInt (83) i

-- | Set a tracer to run on the client. Should be set to the same value as the tracer set on the server.
distributedClientTracer str = NetworkOptionString (90) str

-- | This option is set automatically to communicate the list of supported clients to the active client.
supportedClientVersions str = NetworkOptionString (1000) str

-- | This option is set automatically on all clients loaded externally using the multi-version API.
externalClient = NetworkOptionFlag (1001)

-- | This option tells a child on a multiversion client what transport ID to use.
externalClientTransportId i = NetworkOptionInt (1002) i

