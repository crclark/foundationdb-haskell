{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | NOTE: This file is generated from <https://github.com/apple/foundationdb/blob/master/fdbclient/vexillographer/fdb.options fdb.options>
-- by the generate-options executable in this project.
-- All documentation on the individual options in this namespace comes
-- from FoundationDB's documentation in @fdb.options@.
module FoundationDB.Options.MutationType where
import Data.ByteString.Char8 (ByteString)

{-# DEPRECATED
and "Deprecated in FDB C API"
 #-}
{-# DEPRECATED
or "Deprecated in FDB C API"
 #-}
{-# DEPRECATED
xor "Deprecated in FDB C API"
 #-}
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

-- | Appends ``param`` to the end of the existing value already in the database at the given key (or creates the key and sets the value to ``param`` if the key is empty). This will only append the value if the final concatenated value size is less than or equal to the maximum value size (i.e., if it fits). WARNING: No error is surfaced back to the user if the final value is too large because the mutation will not be applied until after the transaction has been committed. Therefore, it is only safe to use this mutation type if one can guarantee that one will keep the total value size under the maximum size.
appendIfFits bs = MutationTypeBytes (9) bs

-- | Performs a little-endian comparison of byte strings. If the existing value in the database is not present or shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``. The larger of the two values is then stored in the database.
max bs = MutationTypeBytes (12) bs

-- | Performs a little-endian comparison of byte strings. If the existing value in the database is not present, then ``param`` is stored in the database. If the existing value in the database is shorter than ``param``, it is first extended to the length of ``param`` with zero bytes.  If ``param`` is shorter than the existing value in the database, the existing value is truncated to match the length of ``param``. The smaller of the two values is then stored in the database.
min bs = MutationTypeBytes (13) bs

-- | Transforms ``key`` using a versionstamp for the transaction. Sets the transformed key in the database to ``param``. The key is transformed by removing the final four bytes from the key and reading those as a little-Endian 32-bit integer to get a position ``pos``. The 10 bytes of the key from ``pos`` to ``pos + 10`` are replaced with the versionstamp of the transaction used. The first byte of the key is position 0. A versionstamp is a 10 byte, unique, monotonically (but not sequentially) increasing value for each committed transaction. The first 8 bytes are the committed version of the database (serialized in big-Endian order). The last 2 bytes are monotonic in the serialization order for transactions. WARNING: At this time, versionstamps are compatible with the Tuple layer only in the Java, Python, and Go bindings. Also, note that prior to API version 520, the offset was computed from only the final two bytes rather than the final four bytes.
setVersionstampedKey bs = MutationTypeBytes (14) bs

-- | Transforms ``param`` using a versionstamp for the transaction. Sets the ``key`` given to the transformed ``param``. The parameter is transformed by removing the final four bytes from ``param`` and reading those as a little-Endian 32-bit integer to get a position ``pos``. The 10 bytes of the parameter from ``pos`` to ``pos + 10`` are replaced with the versionstamp of the transaction used. The first byte of the parameter is position 0. A versionstamp is a 10 byte, unique, monotonically (but not sequentially) increasing value for each committed transaction. The first 8 bytes are the committed version of the database (serialized in big-Endian order). The last 2 bytes are monotonic in the serialization order for transactions. WARNING: At this time, versionstamps are compatible with the Tuple layer only in the Java, Python, and Go bindings. Also, note that prior to API version 520, the versionstamp was always placed at the beginning of the parameter rather than computing an offset.
setVersionstampedValue bs = MutationTypeBytes (15) bs

-- | Performs lexicographic comparison of byte strings. If the existing value in the database is not present, then ``param`` is stored. Otherwise the smaller of the two values is then stored in the database.
byteMin bs = MutationTypeBytes (16) bs

-- | Performs lexicographic comparison of byte strings. If the existing value in the database is not present, then ``param`` is stored. Otherwise the larger of the two values is then stored in the database.
byteMax bs = MutationTypeBytes (17) bs

-- | Performs an atomic ``compare and clear`` operation. If the existing value in the database is equal to the given value, then given key is cleared.
compareAndClear bs = MutationTypeBytes (20) bs

