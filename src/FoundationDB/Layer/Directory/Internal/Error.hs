module FoundationDB.Layer.Directory.Internal.Error where

import Control.Monad.Error.Class (MonadError (..))
import FoundationDB.Error (DirLayerUserError, Error (..), FDBHsError (..))
import FoundationDB.Transaction (Transaction)

throwDirInternalError :: String -> Transaction a
throwDirInternalError = throwError . Error . DirectoryLayerInternalError

throwDirUserError :: DirLayerUserError -> Transaction a
throwDirUserError = throwError . Error . DirLayerUserError
