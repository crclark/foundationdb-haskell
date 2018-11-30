module FoundationDB.Layer.Directory.Internal.Error where

import Control.Monad.Error.Class (MonadError(..))

import FoundationDB
import FoundationDB.Error (DirLayerUserError, FDBHsError(..))

throwDirInternalError :: String -> Transaction a
throwDirInternalError = throwError . Error . DirectoryLayerInternalError

throwDirUserError :: DirLayerUserError -> Transaction a
throwDirUserError = throwError . Error . DirLayerUserError
