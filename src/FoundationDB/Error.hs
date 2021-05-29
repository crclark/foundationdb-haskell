-- | Types for all errors that can be thrown while using this library.
module FoundationDB.Error (
  Error(..),
  DirLayerUserError(..),
  FDBHsError(..),
  CError(..),
  ConflictRange(..),
  retryable,
  retryableNotCommitted
) where

import FoundationDB.Error.Internal
