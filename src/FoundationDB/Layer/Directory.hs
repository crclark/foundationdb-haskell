-- | The directory layer. The directory layer provides tools for creating a
-- hierarchy of 'Subspace's, which can be operated on efficiently with a
-- directory-like API.
--
-- This implementation of the directory layer does not yet support directory
-- partitions. They will be added in the future.
module FoundationDB.Layer.Directory (
  DirectoryLayer
  , defaultDirLayer
  , newDirectoryLayer
  , Directory
  , Path
  , dirSubspace
  , dirPath
  , dirLayer
  , open
  , createOrOpen
  , move
  , remove
  , exists
  , list
  -- * Advanced usage
  , open'
  , createOrOpen'
) where

import FoundationDB.Layer.Directory.Internal

-- TODO: in the other bindings, there is an abstract directory interface
-- and two implementors of that interface. DirSubspace is the standard one,
-- and DirPartition is the weird one that throws exceptions for most of the
-- functions in the directory interface. In the short term, I think we need only
-- support DirSubspace. In Java, DirPartition inherits from DirSubspace.
-- They both contain an internal DirectoryLayer object that does the real work.
-- We should export a sum type where the constructors are
-- DirSubspace and DirPartition. Or, since DirPartition supports only a subset
-- of operations, make DirSubspace and DirPartition separate types implementing
-- different type classes, so we don't have excessive partiality.




