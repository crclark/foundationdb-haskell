{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StackMachine where

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

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
  | Get { snapshot :: Bool, database :: Bool }
  | GetKey { snapshot :: Bool, database :: Bool}
  | GetRange { snapshot :: Bool, database :: Bool}
  | GetRangeStartsWith { snapshot :: Bool, database :: Bool}
  | GetRangeSelector { snapshot :: Bool, database :: Bool}
  | GetReadVersion { snapshot :: Bool }
  | GetVersionstamp
  | Set { database :: Bool }
  | SetReadVersion
  | Clear { database :: Bool }
  | ClearRange { database :: Bool }
  | ClearRangeStartsWith { database :: Bool }
  | AtomicOp { database :: Bool }
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
  | UnknownOp [Elem]
  deriving Show

parseOp :: ByteString -> Op
parseOp v = case decodeTupleElems v of
  Right [TextElem "PUSH", item]      -> Push (StackItem item)
  Right [TextElem "DUP"            ] -> Dup
  Right [TextElem "EMPTY_STACK"    ] -> EmptyStack
  Right [TextElem "SWAP"           ] -> Swap
  Right [TextElem "POP"            ] -> Pop
  Right [TextElem "SUB"            ] -> Sub
  Right [TextElem "CONCAT"         ] -> Concat
  Right [TextElem "LOG_STACK"      ] -> LogStack
  Right [TextElem "NEW_TRANSACTION"] -> NewTransaction
  Right [TextElem "USE_TRANSACTION"] -> UseTransaction
  Right [TextElem "ON_ERROR"       ] -> OnError
  Right [TextElem "GET", BoolElem snapshot, BoolElem database] -> Get {..}
  Right [TextElem "GET_KEY", BoolElem snapshot, BoolElem database] ->
    GetKey {..}
  Right [TextElem "GET_RANGE", BoolElem snapshot, BoolElem database] ->
    GetRange {..}
  Right [TextElem "GET_RANGE_STARTS_WITH", BoolElem snapshot, BoolElem database]
    -> GetRangeStartsWith {..}
  Right [TextElem "GET_RANGE_SELECTOR", BoolElem snapshot, BoolElem database]
    -> GetRangeSelector {..}
  Right [TextElem "GET_READ_VERSION", BoolElem snapshot] -> GetReadVersion {..}
  Right [TextElem "GET_VERSIONSTAMP"]                    -> GetVersionstamp
  Right [TextElem "SET", BoolElem database]              -> Set {..}
  Right [TextElem "SET_READ_VERSION"]                    -> SetReadVersion
  Right [TextElem "CLEAR"      , BoolElem database]      -> Clear {..}
  Right [TextElem "CLEAR_RANGE", BoolElem database]      -> ClearRange {..}
  Right [TextElem "CLEAR_RANGE_STARTS_WITH", BoolElem database] ->
    ClearRangeStartsWith {..}
  Right [TextElem "ATOMIC_OP", BoolElem database] -> AtomicOp {..}
  Right [TextElem "READ_CONFLICT_RANGE"         ] -> ReadConflictRange
  Right [TextElem "WRITE_CONFLICT_RANGE"        ] -> WriteConflictRange
  Right [TextElem "READ_CONFLICT_KEY"           ] -> ReadConflictKey
  Right [TextElem "WRITE_CONFLICT_KEY"          ] -> WriteConflictKey
  Right [TextElem "DISABLE_WRITE_CONFLICT"      ] -> DisableWriteConflict
  Right [TextElem "COMMIT"                      ] -> Commit
  Right [TextElem "RESET"                       ] -> Reset
  Right [TextElem "CANCEL"                      ] -> Cancel
  Right [TextElem "GET_COMMITTED_VERSION"       ] -> GetCommittedVersion
  Right [TextElem "WAIT_FUTURE"                 ] -> WaitFuture
  Right [TextElem "TUPLE_PACK"                  ] -> TuplePack
  Right [TextElem "TUPLE_PACK_WITH_VERSIONSTAMP"] -> TuplePackWithVersionstamp
  Right [TextElem "TUPLE_UNPACK"                ] -> TupleUnpack
  Right [TextElem "TUPLE_SORT"                  ] -> TupleSort
  Right [TextElem "ENCODE_FLOAT"                ] -> EncodeFloat
  Right [TextElem "ENCODE_DOUBLE"               ] -> EncodeDouble
  Right [TextElem "DECODE_FLOAT"                ] -> DecodeFloat
  Right [TextElem "DECODE_DOUBLE"               ] -> DecodeDouble
  Right [TextElem "START_THREAD"                ] -> StartThread
  Right [TextElem "WAIT_EMPTY"                  ] -> WaitEmpty
  Right unknown -> UnknownOp unknown
  Left  err     -> error $ "Failed to decode tuple: " ++ show v


getOps :: Database -> ByteString -> IO [Op]
getOps db prefix = runTransaction db $ do
  kvs <- getEntireRange $ fromJust $ prefixRange prefix
  return $ map (parseOp . snd) kvs

runMachine :: IORef (Map ByteString TransactionEnv) -> StackMachine -> ResIO ()
runMachine transMap StackMachine {..} = do
  ops <- liftIO $ getOps db transactionName
  undefined

runTests :: Int -> ByteString -> Database -> IO ()
runTests ver prefix db = do
  transMap <- newIORef M.empty
  let machine = StackMachine [] 0 ver prefix db
  runResourceT $ runMachine transMap machine
