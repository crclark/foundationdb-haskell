module FoundationDB.Transaction.Internal where

import Control.Monad.Except
import Control.Monad.Trans.Resource
import FoundationDB.Error
import qualified FoundationDB.Internal.Bindings as FDB

-- | Contains useful options that are not directly exposed by the C API (for
--   options that are, see 'setOption').
data TransactionConfig = TransactionConfig {
  idempotent :: Bool
  -- ^ When set to 'True' (default is 'False'), running the transaction will
  -- retry even on errors where the transaction may have completed successfully.
  -- When 'False', the transaction will retry only when it is guaranteed that
  -- the transaction was not committed.
  , snapshotReads :: Bool
  -- ^ When set to 'True' (default is 'False'), reads will see the effects of
  -- concurrent transactions, removing the default serializable isolation
  -- guarantee. To enable this feature selectively within a transaction,
  -- see 'withSnapshot'.
  } deriving (Show, Read, Eq, Ord)

defaultConfig :: TransactionConfig
defaultConfig = TransactionConfig False False

data TransactionEnv = TransactionEnv {
  cTransaction :: FDB.Transaction
  , conf :: TransactionConfig
  } deriving (Show)

createTransactionEnv :: FDB.Database
                       -> TransactionConfig
                       -> ExceptT Error (ResourceT IO) TransactionEnv
createTransactionEnv db config = do
  (_rk, eTrans) <- allocate (fdbEither $ FDB.databaseCreateTransaction db)
                            (either (const $ return ()) FDB.transactionDestroy)
  liftEither $ fmap (flip TransactionEnv config) eTrans
