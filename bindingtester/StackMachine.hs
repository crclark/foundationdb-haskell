{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module StackMachine where

import Control.Concurrent.MVar
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pretty.Simple
import System.IO.Unsafe (unsafePerformIO)

import FoundationDB
import FoundationDB.Transaction
import FoundationDB.Transaction.Internal
import FoundationDB.Layer.Tuple

-- TODO: stack items can be of many types
-- TODO: stack items need to also hold their instruction number, whatever
--       that is.
data StackItem = StackItem Elem
  deriving Show

data StackMachine = StackMachine
  { stack :: [StackItem]
  , stackLen :: Int
  , version :: Int
  , transactionName :: ByteString
  , db :: Database
  } deriving (Show)

push :: StackMachine -> StackItem -> StackMachine
push st@StackMachine {..} i = st { stack = i : stack, stackLen = stackLen + 1 }

pop :: StackMachine -> Maybe (StackItem, StackMachine)
pop st@StackMachine {..} = case stack of
  []       -> Nothing
  (x : xs) -> Just (x, st { stack = xs, stackLen = stackLen - 1 })

popN :: Int -> StackMachine -> Maybe ([StackItem], StackMachine)
popN n st@StackMachine {..} = if stackLen < n
  then Nothing
  else
    let (popped, rest) = splitAt n stack
    in  Just (popped, st { stack = rest, stackLen = stackLen - n })

-- TODO: it's probably possible to pass this around to avoid making it truly
-- global.
transactions :: MVar (Map ByteString TransactionEnv)
transactions = unsafePerformIO $ newMVar mempty
{-# NOINLINE transactions #-}

data Op =
  Push StackItem
  | Dup
  | EmptyStack
  | Swap
  | Pop
  | Sub
  | Concat
  | LogStack
  | NewTransaction
  | UseTransaction
  | OnError
  | Get
  | GetKey
  | GetRange
  | GetRangeStartsWith
  | GetRangeSelector
  | GetReadVersion
  | GetVersionstamp
  | Set
  | SetReadVersion
  | Clear
  | ClearRange
  | ClearRangeStartsWith
  | AtomicOp
  | ReadConflictRange
  | WriteConflictRange
  | ReadConflictKey
  | WriteConflictKey
  | DisableWriteConflict
  | Commit
  | Reset
  | Cancel
  | GetCommittedVersion
  | WaitFuture
  | TuplePack
  | TuplePackWithVersionstamp
  | TupleUnpack
  | TupleRange
  | TupleSort
  | EncodeFloat
  | EncodeDouble
  | DecodeFloat
  | DecodeDouble
  | StartThread
  | WaitEmpty
  | SnapshotOp Op
  | DatabaseOp Op
  | UnknownOp [Elem]
  deriving Show

isUnknown :: Op -> Bool
isUnknown (UnknownOp _) = True
isUnknown _ = False

-- TODO: lots of repetition with the _DATABASE suffices. Instead of containing
-- a bool deciding if its a database-level op (that should be run in an
-- anonymous transaction, in the case of our bindings), perhaps it would make
-- more sense to wrap these ops in a @DatabaseOp@ constructor to reduce repetition
-- on the execution side, and use a helper function that parses out _DATABASE
-- suffices to reduce repetition on the parsing side.

-- TODO: are the _SNAPSHOT and _DATABASE suffices mutually exclusive?

-- | Parses a parameterless op. In practice, that means any op except for PUSH.
-- If the Op is unknown/unimplemented, returns 'Nothing'.
parseBasicOp :: Text -> Maybe Op
parseBasicOp (T.stripSuffix "_DATABASE" -> Just t) =
  DatabaseOp <$> parseBasicOp t
parseBasicOp (T.stripSuffix "_SNAPSHOT" -> Just t) =
  SnapshotOp <$> parseBasicOp t
parseBasicOp t = case t of
  "DUP" -> Just Dup
  "EMPTY_STACK" -> Just EmptyStack
  "SWAP" -> Just Swap
  "POP" -> Just Pop
  "SUB" -> Just Sub
  "CONCAT" -> Just Concat
  "LOG_STACK" -> Just LogStack
  "NEW_TRANSACTION" -> Just NewTransaction
  "USE_TRANSACTION" -> Just UseTransaction
  "ON_ERROR" -> Just OnError
  "GET" -> Just Get
  "GET_KEY" -> Just GetKey
  "GET_RANGE" -> Just GetRange
  "GET_RANGE_STARTS_WITH" -> Just GetRangeStartsWith
  "GET_RANGE_SELECTOR" -> Just GetRangeSelector
  "GET_READ_VERSION" -> Just GetReadVersion
  "GET_VERSIONSTAMP" -> Just GetVersionstamp
  "SET" -> Just Set
  "SET_READ_VERSION" -> Just SetReadVersion
  "CLEAR" -> Just Clear
  "CLEAR_RANGE" -> Just ClearRange
  "CLEAR_RANGE_STARTS_WITH" -> Just ClearRangeStartsWith
  "ATOMIC_OP" -> Just AtomicOp
  "READ_CONFLICT_RANGE" -> Just ReadConflictRange
  "WRITE_CONFLICT_RANGE" -> Just WriteConflictRange
  "READ_CONFLICT_KEY" -> Just ReadConflictKey
  "WRITE_CONFLICT_KEY" -> Just WriteConflictKey
  "DISABLE_WRITE_CONFLICT" -> Just DisableWriteConflict
  "COMMIT" -> Just Commit
  "RESET" -> Just Reset
  "CANCEL" -> Just Cancel
  "GET_COMMITTED_VERSION" -> Just GetCommittedVersion
  "WAIT_FUTURE" -> Just WaitFuture
  "TUPLE_PACK" -> Just TuplePack
  "TUPLE_PACK_WITH_VERSIONSTAMP" -> Just TuplePackWithVersionstamp
  "TUPLE_UNPACK" -> Just TupleUnpack
  "TUPLE_RANGE" -> Just TupleRange
  "TUPLE_SORT" -> Just TupleSort
  "ENCODE_FLOAT" -> Just EncodeFloat
  "ENCODE_DOUBLE" -> Just EncodeDouble
  "DECODE_FLOAT" -> Just DecodeFloat
  "DECODE_DOUBLE" -> Just DecodeDouble
  "START_THREAD" -> Just StartThread
  "WAIT_EMPTY" -> Just WaitEmpty
  _ -> Nothing

parseOp :: ByteString -> Op
parseOp bs = case decodeTupleElems bs of
  Right [TextElem "PUSH", item] -> Push (StackItem item)
  Right t@[TextElem op] -> fromMaybe (UnknownOp t) (parseBasicOp op)
  Right t -> UnknownOp t
  Left e -> error $ "Error parsing tuple: " ++ show e

getOps :: Database -> ByteString -> IO [Op]
getOps db prefix = runTransaction db $ do
  let prefixTuple = encodeTupleElems [BytesElem prefix]
  kvs <- getEntireRange $ fromJust $ prefixRange prefixTuple
  return $ map (parseOp . snd) kvs

runMachine :: IORef (Map ByteString TransactionEnv) -> StackMachine -> ResIO ()
runMachine transMap StackMachine {..} = do
  ops <- liftIO $ getOps db transactionName
  let numUnk = length (filter isUnknown ops)
  liftIO $ putStrLn $ "Got " ++ show numUnk ++ " unknown ops."
  pPrint ops

runTests :: Int -> ByteString -> Database -> IO ()
runTests ver prefix db = do
  transMap <- newIORef M.empty
  let machine = StackMachine [] 0 ver prefix db
  runResourceT $ runMachine transMap machine
