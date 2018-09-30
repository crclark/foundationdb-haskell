{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}

module StackMachine where

import Data.IORef
import Control.Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (MonadState, StateT)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pretty.Simple
import System.IO.Unsafe (unsafePerformIO)

import FoundationDB
import FoundationDB.Error
import FoundationDB.Transaction
import FoundationDB.Layer.Tuple
-- TODO: are there any things we had to import from internal that should be
-- exposed?
import FoundationDB.Internal.Bindings ( getCFDBError
                                      , tupleKeySelector
                                      , CFDBError (..)
                                      , keySelectorBytes)

strinc :: ByteString -> ByteString
strinc = prefixRangeEnd

-- TODO: stack items can be of many types
data StackItem = StackItem Elem Int
  deriving Show

pattern StackBytes :: ByteString -> StackItem
pattern StackBytes x <- StackItem (BytesElem x) _

pattern StackInt :: Int -> StackItem
pattern StackInt x <- StackItem (IntElem x) _

data StackMachine = StackMachine
  { stack :: [StackItem]
  , stackLen :: Int
  , version :: Int
  , transactionName :: ByteString
  , db :: Database
  , prefixTuple :: ByteString
  } deriving (Show)

push :: MonadState StackMachine m => StackItem -> m ()
push i = do
  st@StackMachine {..} <- State.get
  State.put st { stack = i : stack, stackLen = stackLen + 1 }

pop :: MonadState StackMachine m => m (Maybe StackItem)
pop = do
  st@StackMachine {..} <- State.get
  case stack of
    []       -> return Nothing
    (x : xs) -> do
      State.put st { stack = xs, stackLen = stackLen - 1 }
      return $ Just x

popN :: MonadState StackMachine m => Int -> m (Maybe [StackItem])
popN n = do
  st@StackMachine {..} <- State.get
  if stackLen < n
    then return Nothing
    else do
    let (popped, rest) = splitAt n stack
    State.put st { stack = rest, stackLen = stackLen - n }
    return $ Just popped

peek :: MonadState StackMachine m => m (Maybe StackItem)
peek = do
  st <- State.get
  return $ case stack st of
    [] -> Nothing
    (x:_) -> Just x

swap :: MonadState StackMachine m => Int -> m ()
swap n = do
  st@StackMachine{..} <- State.get
  State.put $ case splitAt n stack of
    (x:xs, y:ys) -> st {stack = (y:xs) ++ (x:ys)}
    _ -> st

-- TODO: it's probably possible to pass this around to avoid making it truly
-- global.
transactions :: IORef (Map ByteString TransactionEnv)
transactions = unsafePerformIO $ newIORef mempty
{-# NOINLINE transactions #-}

updateTransactions :: MonadIO m
                   => (Map ByteString TransactionEnv -> Map ByteString TransactionEnv)
                   -> m ()
updateTransactions f = void $ liftIO $ atomicModifyIORef' transactions (\x -> (x, f x))

getEnv :: (MonadState StackMachine m, MonadIO m)
       => m TransactionEnv
getEnv = do
  st <- State.get
  m <- liftIO $ readIORef transactions
  return $ m M.! (transactionName st)

warnEmptyStack :: MonadIO m => Op -> m ()
warnEmptyStack op = liftIO $ putStrLn $ "WARN: Empty stack for op: " ++ show op

warnUnexpectedState :: MonadIO m => Op -> m ()
warnUnexpectedState op =
  liftIO $ putStrLn $ "WARN: Bad stack state for op: " ++ show op

bubbleError :: MonadState StackMachine m => Int -> Error -> m ()
bubbleError i (CError err) =
  let errCode = getCFDBError $ toCFDBError err
      errTuple = [BytesElem "ERROR", BytesElem (BS.pack (show errCode))]
      packedErrTuple = encodeTupleElems errTuple
      in push (StackItem (BytesElem packedErrTuple) i)
bubbleError _ _ = error "Can't bubble FDBHaskell error"

popKeySelector :: MonadState StackMachine m
               => m (Maybe (KeySelector, ByteString))
popKeySelector = popN 4 >>= \case
  Just [ StackBytes k
       , StackInt orEqual
       , StackInt offst
       , StackBytes prefix] -> let
          orEqual' = orEqual == 1
          in return $ Just (tupleKeySelector (k, orEqual', offst), prefix)
  _ -> return Nothing

popRangeArgs :: MonadState StackMachine m
             => m (Maybe (Range, FDBStreamingMode))
popRangeArgs = popN 5 >>= \case
  Just [ StackBytes begin
       , StackBytes end
       , StackInt limit
       , StackInt rev
       , StackInt mode] -> return $ Just (Range {
         rangeBegin = FirstGreaterOrEq begin
         , rangeEnd = FirstGreaterOrEq end
         , rangeLimit = Just limit
         , rangeReverse = rev == 1
       }, toEnum (mode + 1))
  _ -> return Nothing

popRangeStartsWith :: MonadState StackMachine m
                   => m (Maybe (Range, FDBStreamingMode))
popRangeStartsWith = popN 4 >>= \case
  Just [ StackBytes prefix
       , StackInt limit
       , StackInt rev
       , StackInt mode] -> return $ do
         r <- prefixRange prefix
         let r' = r {
           rangeLimit = Just limit,
           rangeReverse = rev == 1
           }
         return (r', toEnum (mode + 1))
  _ -> return Nothing

popRangeSelector :: MonadState StackMachine m
                 => m (Maybe (Range, FDBStreamingMode, ByteString))
popRangeSelector = popN 10 >>= \case
  Just [ StackBytes beginK
       , StackInt beginOrEqual
       , StackInt beginOffset
       , StackBytes endK
       , StackInt endOrEqual
       , StackInt endOffset
       , StackInt limit
       , StackInt rev
       , StackInt mode
       , StackBytes prefix] -> do
        let beginKS = tupleKeySelector (beginK, beginOrEqual == 1, beginOffset)
        let endKS = tupleKeySelector (endK, endOrEqual == 1, endOffset)
        let r = Range {
                rangeBegin = beginKS
                , rangeEnd = endKS
                , rangeLimit = Just limit
                , rangeReverse = rev == 1
                }
        return $ Just (r, toEnum (mode + 1), prefix)
  _ -> return Nothing

rangeList :: RangeResult -> Transaction [(ByteString, ByteString)]
rangeList (RangeDone xs) = return xs
rangeList (RangeMore xs more) = do
  rr <- await more
  ys <- rangeList rr
  return $ xs ++ ys

-- TODO: lots of repetition below. Pattern synonym for the StackItem noise?

-- | Runs a transaction in the current env, handling transaction errors as
-- specified by the bindings tester spec. If no error occurs, passes the result
-- of the transaction to the given handler function.
bubblingError :: Int
            -- ^ instruction number
            -> Transaction a
            -> (a -> StateT StackMachine ResIO ())
            -> StateT StackMachine ResIO ()
bubblingError i t handle = do
  env <- getEnv
  liftIO (onEnv env t) >>= \case
    Left err -> bubbleError i err
    Right x -> handle x

-- | Runs a single operation on a stack machine.
step :: Int
     -- ^ instruction number
     -> Op
     -> StateT StackMachine ResIO ()

step _ (Push item) = push item

step _ Dup = peek >>= \case
  Nothing -> return ()
  Just x -> warnEmptyStack Dup >> push x

step _ EmptyStack = do
  st <- State.get
  State.put st {stack = [], stackLen = 0}

step _ Swap = pop >>= \case
  Just (StackItem (IntElem n) _) -> swap n
  _ -> warnUnexpectedState Swap

step _ Pop = pop >>= \case
  Just _ -> return ()
  _ -> warnUnexpectedState Pop

step i Sub = popN 2 >>= \case
  Just [StackItem (IntElem x) _, StackItem (IntElem y) _] ->
    push $ StackItem (IntElem (x - y)) i
  _ -> warnUnexpectedState Sub

step i Concat = popN 2 >>= \case
  Just [StackItem (BytesElem x) _, StackItem (BytesElem y) _] ->
    push $ StackItem (BytesElem (x <> y)) i
  Just [StackItem (TextElem x) _, StackItem (TextElem y) _] ->
    push $ StackItem (TextElem (x <> y)) i
  _ -> warnUnexpectedState Concat

step i LogStack = do
  Just (StackItem (BytesElem prfx) _) <- pop
  st <- State.get
  go (db st) prfx (stack st) (stackLen st)

  where
    go _ _ [] 0 = do
      st <- State.get
      State.put st {stack = [], stackLen = 0}
    go _ _ [] _ = error "impossible case in StackMachine"
    go db prfx (StackItem x _:xs) n = do
      liftIO $ runTransaction db $ do
        let k = prfx <> encodeTupleElems [IntElem n, IntElem i]
        let v = BS.take 40000 $ encodeTupleElems [x]
        set k v
      go db prfx xs (n-1)


step _ NewTransaction = do
  st <- State.get
  envE <- State.lift $
          runExceptT $
          createTransactionEnv (db st) (TransactionConfig False False)
  case envE of
    Left err -> error (show err)
    Right env -> do
      let k = transactionName st
      _ <- liftIO $ atomicModifyIORef' transactions $ \m -> (m, M.insert k env m)
      return ()

-- TODO: duplication with NewTransaction case
-- TODO: spec says to create transaction if it doesn't exist, but this always
-- creates a transaction, letting it get GC'ed if one already exists. Kind of
-- clumsy.
step _ UseTransaction = pop >>= \case
  Just (StackItem (BytesElem txnName) _) -> do
    st <- State.get
    envE <- State.lift $
            runExceptT $
            createTransactionEnv (db st) (TransactionConfig False False)
    case envE of
      Left err -> error (show err)
      Right env -> do
        void $ liftIO $ atomicModifyIORef' transactions $ \m ->
          let m' = if M.member txnName m
                      then m
                      else M.insert txnName env m
              in (m, m')
        State.put st {transactionName = txnName}
  _ -> warnUnexpectedState UseTransaction

step i OnError = pop >>= \case
  Just (StackItem (BytesElem errCode) _) -> do
    env <- getEnv
    let err = CError (toError (CFDBError $ read $ BS.unpack errCode))
    liftIO (onEnv env (onError err)) >>= \case
      Left err' -> bubbleError i err'
      Right () -> return ()
  _ -> warnUnexpectedState OnError

step i Get = pop >>= \case
  Just (StackItem (BytesElem k) _) ->
    bubblingError i (get k >>= await) $ \case
      Nothing -> push (StackItem (BytesElem "RESULT_NOT_PRESENT") i)
      Just v -> push (StackItem (BytesElem v) i)
  _ -> warnUnexpectedState Get

step i GetKey = popKeySelector >>= \case
  Just (sel, prefix) -> do
    let k = keySelectorBytes sel
    bubblingError i (get k >>= await) $ \case
      Nothing -> push (StackItem (BytesElem "RESULT_NOT_PRESENT") i)
      Just v
        | BS.isPrefixOf prefix v -> push (StackItem (BytesElem v) i)
        | v < prefix -> push (StackItem (BytesElem prefix) i)
        | v > prefix -> push (StackItem (BytesElem (strinc prefix)) i)
        | otherwise -> error "impossible case for GetKey"
  _ -> warnEmptyStack GetKey

step i GetRange = popRangeArgs >>= \case
  Just (range, mode) -> do
    let flatten [] = []
        flatten ((k,v):kvs) = k : v : flatten kvs
    bubblingError i (getRange' range mode >>= await >>= rangeList) $ \xs -> do
      let tuple = encodeTupleElems $ map BytesElem $ flatten xs
      push (StackItem (BytesElem tuple) i)
  _ -> warnEmptyStack GetRange

step i GetRangeStartsWith = popRangeStartsWith >>= \case
  Just (range, mode) -> do
    let flatten [] = []
        flatten ((k,v):kvs) = k : v : flatten kvs
    bubblingError i (getRange' range mode >>= await >>= rangeList) $ \xs -> do
      let tuple = encodeTupleElems $ map BytesElem $ flatten xs
      push (StackItem (BytesElem tuple) i)
  _ -> warnUnexpectedState GetRangeStartsWith

step i GetRangeSelector = popRangeSelector >>= \case
  Just (range, mode, prefix) -> do
    let flatten [] = []
        flatten ((k,v):kvs) = k : v : flatten kvs
    bubblingError i (getRange' range mode >>= await >>= rangeList) $ \xs -> do
      let tuple = encodeTupleElems $ map BytesElem $ flatten $ filter (BS.isPrefixOf prefix . fst) xs
      push (StackItem (BytesElem tuple) i)
  _ -> warnUnexpectedState GetRangeSelector

step i GetReadVersion = undefined i

step i GetVersionstamp = undefined i

step i Set = undefined i

step i SetReadVersion = undefined i

step i Clear = undefined i

step i ClearRange = undefined i

step i ClearRangeStartsWith = undefined i

step i AtomicOp = undefined i

step i ReadConflictRange = undefined i

step i WriteConflictRange = undefined i

step i ReadConflictKey = undefined i

step i WriteConflictKey = undefined i

step i DisableWriteConflict = undefined i

step i Commit = undefined i

step i Reset = undefined i

step i Cancel = undefined i

step i GetCommittedVersion = undefined i

step i WaitFuture = undefined i

step i TuplePack = undefined i

step i TuplePackWithVersionstamp = undefined i

step i TupleUnpack = undefined i

step i TupleRange = undefined i

step i TupleSort = undefined i

step i EncodeFloat = undefined i

step i EncodeDouble = undefined i

step i DecodeFloat = undefined i

step i DecodeDouble = undefined i

step i StartThread = undefined i

step i WaitEmpty = undefined i

step i (SnapshotOp op) = do
  -- TODO: find a better way to do this that's thread safe.
  -- The spec doesn't make it clear
  -- as to whether separate threads will ever operate on the same
  -- transaction, so this might not be safe.
  st <- State.get
  updateTransactions $ M.adjust (\env -> env {envConf = TransactionConfig False True})
                                (transactionName st)
  step i op
  updateTransactions $ M.adjust (\env -> env {envConf = TransactionConfig False False})
                                (transactionName st)

step i (DatabaseOp _) = undefined i

step _ (UnknownOp x) = error $ "tried to execute unknown op: " ++ show x

-- TODO: split this into a sum of a couple different Op types.
-- SnapshotOp (Push x) makes no sense, for example.
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
  Right [TextElem "PUSH", item] -> Push (StackItem item 1337)
  Right t@[TextElem op] -> fromMaybe (UnknownOp t) (parseBasicOp op)
  Right t -> UnknownOp t
  Left e -> error $ "Error parsing tuple: " ++ show e

getOps :: Database -> ByteString -> IO [Op]
getOps db prefix = runTransaction db $ do
  let prefixTuple = encodeTupleElems [BytesElem prefix]
  kvs <- getEntireRange $ fromJust $ prefixRange prefixTuple
  return $ map (parseOp . snd) kvs

runMachine :: IORef (Map ByteString TransactionEnv) -> StackMachine -> ResIO ()
runMachine _transMap StackMachine {..} = do
  ops <- liftIO $ getOps db transactionName
  let numUnk = length (filter isUnknown ops)
  liftIO $ putStrLn $ "Got " ++ show numUnk ++ " unknown ops."
  pPrint ops

runTests :: Int -> ByteString -> Database -> IO ()
runTests ver prefix db = do
  transMap <- newIORef M.empty
  let machine = StackMachine [] 0 ver prefix db (encodeTupleElems [BytesElem prefix])
  runResourceT $ runMachine transMap machine
