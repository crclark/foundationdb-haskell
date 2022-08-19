-- | The directory layer provides tools for creating a
-- hierarchy of 'Subspace's, which can be operated on efficiently with a
-- directory-like API. This is one of the official layers supported by all
-- language bindings. See the
-- <https://apple.github.io/foundationdb/developer-guide.html#directories official FoundationDB documentation>
-- for more information.
--
-- This implementation of the directory layer does not yet support directory
-- partitions. They will be added in the future.
module FoundationDB.Layer.Directory
  ( DirectoryLayer,
    defaultDirLayer,
    newDirectoryLayer,
    Directory,
    Path,
    dirSubspace,
    dirPath,
    dirLayer,
    open,
    createOrOpen,
    move,
    remove,
    exists,
    list,

    -- * Advanced usage
    open',
    createOrOpen',
  )
where

import FoundationDB.Layer.Directory.Internal
