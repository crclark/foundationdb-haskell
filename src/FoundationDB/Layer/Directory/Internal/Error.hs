module FoundationDB.Layer.Directory.Internal.Error (throwDirError) where

import Control.Monad.Error.Class (MonadError(..))

import FoundationDB
import FoundationDB.Error (FDBHsError(..))

throwDirError :: String -> Transaction a
throwDirError = throwError . Error . DirectoryLayerError
