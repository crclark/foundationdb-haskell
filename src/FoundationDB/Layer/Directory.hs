{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: look for simplifications for the interface exported here. See
-- https://github.com/apple/foundationdb/blob/master/bindings/go/src/fdb/directory/directory.go
-- for ideas to simplify.
module FoundationDB.Layer.Directory (
  open
  , createOrOpen
  , move
  , remove
  , exists
  , list
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




