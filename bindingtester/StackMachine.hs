{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module StackMachine where

import Data.Char (isAscii, isPrint)
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
import Data.Sequence(Seq(Empty,(:<|)))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts(IsList(..))
import Text.Printf (printf)

import FoundationDB
import FoundationDB.Error
import FoundationDB.Transaction
import FoundationDB.Layer.Tuple
import FoundationDB.Internal.Bindings ( getCFDBError
                                      , tupleKeySelector
                                      , CFDBError (..)
                                      , keySelectorBytes)
import qualified FoundationDB.Options as Opts
import FoundationDB.Versionstamp

strinc :: ByteString -> ByteString
strinc = prefixRangeEnd

flatten :: Seq (a,a) -> Seq a
flatten Empty = Empty
flatten ((k,v):<|kvs) = k :<| v :<| flatten kvs

type InstructionNum = Integer

-- | attempts to mimic the behavior of Python's print function on byte strings.
pythonShow :: ByteString -> String
pythonShow = concatMap toStr . BS.unpack
  where toStr c | isAscii c && isPrint c = [c]
                | c == toEnum 0 = "\\x00"
                | otherwise = '\\' : tail (printf "%#0.2x" c)

data StackItem =
  StackItem Elem InstructionNum
  | StackVersionstampFuture (FutureIO (Either Error TransactionVersionstamp))
                            InstructionNum
  deriving Show

pattern StackBytes :: ByteString -> StackItem
pattern StackBytes x <- StackItem (Bytes x) _

pattern StackInt :: Integer -> StackItem
pattern StackInt x <- StackItem (Int x) _

data StackMachine = StackMachine
  { stack :: [StackItem]
  , stackLen :: Int
  , version :: Int
  , transactionName :: ByteString
  , transactions :: IORef (Map ByteString TransactionEnv)
  -- ^ Global transactions map. All instances of this point at the same
  -- underlying object.
  , db :: Database
  , prefixTuple :: ByteString
  }

instance Show StackMachine where
  show StackMachine{..} =
    "StackMachine "
    ++ unwords [
      show stack
      , show stackLen
      , show version
      , show transactionName
      , "<IORef>"
      , show db
      , show prefixTuple
    ]

getLastVersion :: MonadState StackMachine m => m Int
getLastVersion = do
  StackMachine {..} <- State.get
  return version

setLastVersion :: MonadState StackMachine m => Int -> m ()
setLastVersion v = do
  st@StackMachine {..} <- State.get
  State.put st {version = v}

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

updateTransactions :: (MonadState StackMachine m, MonadIO m)
                   => (Map ByteString TransactionEnv -> Map ByteString TransactionEnv)
                   -> m ()
updateTransactions f = do
  StackMachine{..} <- State.get
  liftIO $ atomicModifyIORef' transactions (\x -> (f x, ()))

getEnv :: (MonadState StackMachine m, MonadIO m)
       => m TransactionEnv
getEnv = do
  st <- State.get
  m <- liftIO $ readIORef $ transactions st
  return $ m M.! transactionName st

withAnonTransaction :: InstructionNum
                    -> StateT StackMachine ResIO ()
                    -> StateT StackMachine ResIO ()
withAnonTransaction i a = do
  st <- State.get
  anonE <- State.lift $
          runExceptT $
          createTransactionEnv (db st) defaultConfig
  case anonE of
    Left err -> error (show err)
    Right anon -> do
      updateTransactions $ M.insert "anon" anon
      State.put st {transactionName = "anon"}
      res <- a
      bubblingError i (commitFuture >>= await) $ \_ -> do
        State.put st
        return res

errorEmptyStack :: MonadIO m => InstructionNum -> Op -> m ()
errorEmptyStack i op = error $ "Empty stack for op: " ++ show op
                               ++ "\ninstruction number: " ++ show i

errorUnexpectedState :: (Show a, MonadState StackMachine m, MonadIO m)
                     => InstructionNum
                     -> a
                     -> Op
                     -> m ()
errorUnexpectedState i x op =
  error $ "Bad stack state " ++ show x
          ++ "\nfor op: " ++ show op
          ++ "\ninstruction number: " ++ show i

bubbleError :: (MonadState StackMachine m, MonadIO m)
            => InstructionNum
            -> Error
            -> m ()
bubbleError i (CError err) =
  let errCode = getCFDBError $ toCFDBError err
      errTuple = [Bytes "ERROR", Bytes (BS.pack (show errCode))]
      packedErrTuple = encodeTupleElems (errTuple :: [Elem])
      in do
        liftIO $ putStrLn $ "### pushing bubbled error " ++ show errTuple
                            ++ " for instruction number " ++ show i
        push (StackItem (Bytes packedErrTuple) i)
bubbleError _ _ = error "Internal FDBHaskell error"

popKeySelector :: MonadState StackMachine m
               => m (Maybe (KeySelector, ByteString))
popKeySelector = popN 4 >>= \case
  Just [ StackBytes k
       , StackInt orEqual
       , StackInt offst
       , StackBytes prefix] -> let
          orEqual' = orEqual == 1
          in return $ Just ( tupleKeySelector (k, orEqual', fromIntegral offst)
                           , prefix)
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
         , rangeLimit = Just $ fromIntegral limit
         , rangeReverse = rev == 1
       }, toEnum (fromIntegral mode))
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
           rangeLimit = Just $ fromIntegral limit,
           rangeReverse = rev == 1
           }
         return (r', toEnum (fromIntegral mode))
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
        let beginKS = tupleKeySelector ( beginK
                                       , beginOrEqual == 1
                                       , fromIntegral beginOffset)
        let endKS = tupleKeySelector ( endK
                                     , endOrEqual == 1
                                     , fromIntegral endOffset)
        let r = Range {
                rangeBegin = beginKS
                , rangeEnd = endKS
                , rangeLimit = Just $ fromIntegral limit
                , rangeReverse = rev == 1
                }
        return $ Just (r, toEnum $ fromIntegral mode, prefix)
  _ -> return Nothing

popAtomicOp :: MonadState StackMachine m
            => m (Maybe (AtomicOp, ByteString, ByteString))
popAtomicOp = popN 3 >>= \case
  Just [ StackBytes opBytes
       , StackBytes k
       , StackBytes v] -> case parse opBytes of
        Just op -> return $ Just (op, k, v)
        Nothing -> error $ "unknown op: " ++ show opBytes
  _ -> return Nothing

  where parse "ADD" = Just Add
        parse "AND" = Just And
        parse "BIT_AND" = Just BitAnd
        parse "OR" = Just Or
        parse "BIT_OR" = Just BitOr
        parse "XOR" = Just Xor
        parse "BIT_XOR" = Just BitXor
        parse "MAX" = Just Max
        parse "MIN" = Just Min
        parse "SET_VERSIONSTAMPED_KEY" = Just SetVersionstampedKey
        parse "SET_VERSIONSTAMPED_VALUE" = Just SetVersionstampedValue
        parse "BYTE_MIN" = Just ByteMin
        parse "BYTE_MAX" = Just ByteMax
        parse _ = Nothing

rangeList :: RangeResult -> Transaction (Seq (ByteString, ByteString))
rangeList (RangeDone xs) = return xs
rangeList (RangeMore xs more) = do
  rr <- await more
  ys <- rangeList rr
  return $ xs <> ys

-- | Runs a transaction in the current env, handling transaction errors as
-- specified by the bindings tester spec. If no error occurs, passes the result
-- of the transaction to the given handler function.
bubblingError :: InstructionNum
            -> Transaction a
            -> (a -> StateT StackMachine ResIO ())
            -> StateT StackMachine ResIO ()
bubblingError i t handle = do
  env <- getEnv
  liftIO (onEnv env t) >>= \case
    Left err -> bubbleError i err
    Right x -> handle x

-- | Pushes a @RESULT_NOT_PRESENT@ bytestring for the given instruction number.
resultNotPresent :: (MonadState StackMachine m)
                 => InstructionNum
                 -> m ()
resultNotPresent i = push (StackItem (Bytes "RESULT_NOT_PRESENT") i)

-- | Handles pushing @RESULT_NOT_PRESENT@ for @_DATABASE@ operations that don't
-- return results.
finishDBOp :: (MonadState StackMachine m) => InstructionNum -> Op -> m ()
finishDBOp i Set = resultNotPresent i
finishDBOp i Clear = resultNotPresent i
finishDBOp i ClearRange = resultNotPresent i
finishDBOp i ClearRangeStartsWith = resultNotPresent i
finishDBOp i AtomicOp = resultNotPresent i
finishDBOp _ _ = return ()

-- | Runs a single operation on a stack machine.
step :: InstructionNum
     -> Op
     -> StateT StackMachine ResIO ()

step _ (Push item) = push item

step i Dup = peek >>= \case
  Nothing -> errorEmptyStack i Dup
  Just x -> push x

step _ EmptyStack = do
  st <- State.get
  State.put st {stack = [], stackLen = 0}

step i Swap = pop >>= \case
  Just (StackItem (Int n) _) -> swap $ fromIntegral n
  x -> errorUnexpectedState i x Swap

step i Pop = pop >>= \case
  Just _ -> return ()
  x -> errorUnexpectedState i x Pop

step i Sub = popN 2 >>= \case
  Just [StackItem (Int x) _, StackItem (Int y) _] ->
    push $ StackItem (Int (x - y)) i
  x -> errorUnexpectedState i x Sub

step i Concat = popN 2 >>= \case
  Just [StackItem (Bytes x) _, StackItem (Bytes y) _] ->
    push $ StackItem (Bytes (x <> y)) i
  Just [StackItem (Text x) _, StackItem (Text y) _] ->
    push $ StackItem (Text (x <> y)) i
  x -> errorUnexpectedState i x Concat

step i LogStack = do
  Just (StackItem (Bytes prfx) _) <- pop
  st <- State.get
  go (db st) prfx (stack st) (stackLen st)

  where
    go _ _ [] 0 = do
      st <- State.get
      State.put st {stack = [], stackLen = 0}
    go _ _ [] _ = error "impossible case in StackMachine"
    go db prfx (StackItem x _:xs) n = do
      liftIO $ runTransaction db $ do
        let k = prfx <> encodeTupleElems [Int $ fromIntegral n, Int i]
        let v = BS.take 40000 $ encodeTupleElems [x]
        set k v
      go db prfx xs (n-1)
    go _ _ (StackVersionstampFuture _ _:_) _ =
      error "Tried to log a versionstamp future. It should have been awaited."

step _ NewTransaction = do
  st <- State.get
  envE <- State.lift $
          runExceptT $
          createTransactionEnv (db st) defaultConfig
  case envE of
    Left err -> error (show err)
    Right env -> do
      let k = transactionName st
      updateTransactions $ M.insert k env

-- TODO: duplication with NewTransaction case
-- TODO: spec says to create transaction if it doesn't exist, but this always
-- creates a transaction, letting it get GC'ed if one already exists. Kind of
-- clumsy.
step i UseTransaction = pop >>= \case
  Just (StackItem (Bytes txnName) _) -> do
    st <- State.get
    envE <- State.lift $
            runExceptT $
            createTransactionEnv (db st) defaultConfig
    case envE of
      Left err -> error (show err)
      Right env -> do
        void $ liftIO $ atomicModifyIORef' (transactions st) $ \m ->
          let m' = if M.member txnName m
                      then m
                      else M.insert txnName env m
              in (m, m')
        State.put st {transactionName = txnName}
  x -> errorUnexpectedState i x UseTransaction

step i OnError = pop >>= \case
  Just (StackItem (Int errCode) _) -> do
    env <- getEnv
    let cErr = CFDBError $ fromIntegral errCode
    case toError cErr of
      Just e -> liftIO (onEnv env (onError (CError e))) >>= \case
        Left reRaised -> bubbleError i reRaised
        Right () -> resultNotPresent i
      -- Our CError type doesn't include success, which means it's impossible
      -- to pass success to transaction_on_error. However, the stack tester
      -- will ask us to, so we need to simulate the error on_error would return
      -- here.
      Nothing -> bubbleError i (CError ClientInvalidOperation)
  x -> errorUnexpectedState i x OnError

step i Get = pop >>= \case
  Just (StackItem (Bytes k) _) ->
    bubblingError i (get k >>= await) $ \case
      Nothing -> resultNotPresent i
      Just v -> push (StackItem (Bytes v) i)
  x -> errorUnexpectedState i x Get

step i GetKey = popKeySelector >>= \case
  Just (sel, prefix) -> do
    let k = keySelectorBytes sel
    bubblingError i (get k >>= await) $ \case
      Nothing -> resultNotPresent i
      Just v
        | BS.isPrefixOf prefix v -> push (StackItem (Bytes v) i)
        | v < prefix -> push (StackItem (Bytes prefix) i)
        | v > prefix -> push (StackItem (Bytes (strinc prefix)) i)
        | otherwise -> error "impossible case for GetKey"
  _ -> errorEmptyStack i GetKey

step i GetRange = popRangeArgs >>= \case
  Just (range, mode) -> do
    bubblingError i (getRange' range mode >>= await >>= rangeList) $ \xs -> do
      let tuple = encodeTupleElems $ toList $ fmap Bytes $ flatten xs
      push (StackItem (Bytes tuple) i)
  _ -> errorEmptyStack i GetRange

step i GetRangeStartsWith = popRangeStartsWith >>= \case
  Just (range, mode) -> do
    bubblingError i (getRange' range mode >>= await >>= rangeList) $ \xs -> do
      let tuple = encodeTupleElems $ fmap Bytes $ flatten xs
      push (StackItem (Bytes tuple) i)
  x -> errorUnexpectedState i x GetRangeStartsWith

step i GetRangeSelector = popRangeSelector >>= \case
  Just (range, mode, prefix) -> do
    bubblingError i (getRange' range mode >>= await >>= rangeList) $ \xs -> do
      let tuple = encodeTupleElems
                  $ fmap Bytes
                  $ flatten
                  $ Seq.filter (BS.isPrefixOf prefix . fst) xs
      push (StackItem (Bytes tuple) i)
  x -> errorUnexpectedState i x GetRangeSelector

step i GetReadVersion =
  bubblingError i (getReadVersion >>= await) $ \ver -> do
    setLastVersion ver
    push (StackItem (Bytes "GOT_READ_VERSION") i)

step i GetVersionstamp =
  bubblingError i getVersionstamp $ \futVs ->
    push (StackVersionstampFuture futVs i)

step i Set = popN 2 >>= \case
  Just [ StackBytes k
       , StackBytes v] ->do
        liftIO $ putStrLn $ "### " ++ show i
                            ++ " Setting " ++ pythonShow k
                            ++ " (tuple: " ++ show (decodeTupleElems k) ++ " )"
                            ++ " to " ++ pythonShow v
        bubblingError i (set k v) return
  x -> errorUnexpectedState i x Set

step i SetReadVersion = do
  v <- getLastVersion
  bubblingError i (setReadVersion v) return

step i Clear = pop >>= \case
  Just (StackBytes k) ->
    bubblingError i (clear k) return
  x -> errorUnexpectedState i x Clear

step i ClearRange = popN 2 >>= \case
  Just [ StackBytes begin
       , StackBytes end ] ->
        bubblingError i (clearRange begin end) return
  x -> errorUnexpectedState i x ClearRange

step i ClearRangeStartsWith = pop >>= \case
  Just (StackBytes prefix) ->
    bubblingError i (clearRange prefix (prefixRangeEnd prefix)) return
  x -> errorUnexpectedState i x ClearRangeStartsWith

step i AtomicOp = popAtomicOp >>= \case
  Just (op, k, v) ->
    bubblingError i (atomicOp op k v) return
  x@Nothing -> errorUnexpectedState i x AtomicOp

step i ReadConflictRange = popN 2 >>= \case
  Just [ StackBytes begin
       , StackBytes end] -> do
    bubblingError i (addConflictRange begin end ConflictRangeTypeRead) return
    push (StackItem (Bytes "SET_CONFLICT_RANGE") i)
  x -> errorUnexpectedState i x ReadConflictRange

step i WriteConflictRange = popN 2 >>= \case
  Just [ StackBytes begin
       , StackBytes end] -> do
    bubblingError i (addConflictRange begin end ConflictRangeTypeWrite) return
    push (StackItem (Bytes "SET_CONFLICT_RANGE") i)
  x -> errorUnexpectedState i x WriteConflictRange

step i ReadConflictKey = pop >>= \case
  Just (StackBytes k) -> do
    bubblingError i (addReadConflictKey k) return
    push (StackItem (Bytes "SET_CONFLICT_KEY") i)
  x -> errorUnexpectedState i x ReadConflictKey

step i WriteConflictKey = pop >>= \case
  Just (StackBytes k) -> do
    bubblingError i (addWriteConflictKey k) return
    push (StackItem (Bytes "SET_CONFLICT_KEY") i)
  x -> errorUnexpectedState i x WriteConflictKey

step i DisableWriteConflict =
  bubblingError i (setOption Opts.nextWriteNoWriteConflictRange) return

step i Commit =
  bubblingError i (commitFuture >>= await) (const $ resultNotPresent i)

step i Reset =
  bubblingError i reset return

step i Cancel =
  bubblingError i cancel return

step i GetCommittedVersion =
  bubblingError i getCommittedVersion $ \_ ->
    push (StackItem (Bytes "GOT_COMMITTED_VERSION") i)

step i WaitFuture = do
  x <- pop
  case x of
    Nothing -> errorUnexpectedState i x WaitFuture
    Just (StackVersionstampFuture f _) -> do
      res <- liftIO $ awaitIO f
      case res of
        Left err -> error $ show err
        Right (Left err) -> error $ show err
        Right (Right vs) -> push $ StackItem (Bytes (encodeTransactionVersionstamp vs)) i
    Just y -> push y

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
  updateTransactions $ M.adjust (\env -> env {envConf = TransactionConfig False True 5})
                                (transactionName st)
  step i op
  updateTransactions $ M.adjust (\env -> env {envConf = defaultConfig})
                                (transactionName st)

step i (DatabaseOp op) = withAnonTransaction i (step i op) >> finishDBOp i op

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

debugDisplay :: Op -> String
debugDisplay (Push (StackBytes bs)) = "Push " ++ pythonShow bs
debugDisplay x = show x

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

parseOp :: InstructionNum -> ByteString -> Op
parseOp idx bs = case decodeTupleElems bs of
  Right [Text "PUSH", item] -> Push (StackItem item idx)
  Right t@[Text op] -> fromMaybe (UnknownOp t) (parseBasicOp op)
  Right t -> UnknownOp t
  Left e -> error $ "Error parsing tuple: " ++ show e

getOps :: Database -> ByteString -> IO (Seq Op)
getOps db prefix = runTransaction db $ do
  let prefixTuple = encodeTupleElems [Bytes prefix]
  kvs <- getEntireRange $ fromJust $ prefixRange prefixTuple
  return $ fmap (\(idx, (_k, v)) -> parseOp idx v) (Seq.zip (fromList [0..]) kvs)

runMachine :: StackMachine -> ResIO ()
runMachine st@StackMachine {..} = do
  ops <- liftIO $ getOps db transactionName
  let numUnk = Seq.length (Seq.filter isUnknown ops)
  liftIO $ putStrLn $ "Got " ++ show numUnk ++ " unknown ops."
  liftIO $ mapM_ (\(i, x) -> putStrLn $ show i ++ " " ++ x)
                 (zip [(0 :: Int)..] (toList $ fmap debugDisplay ops))
  State.evalStateT (forM_ (zip [0..] (toList ops)) (uncurry step)) st

runTests :: Int -> ByteString -> Database -> IO ()
runTests ver prefix db = do
  putStrLn $ "Starting stack machine using prefix " ++ show prefix
  transMap <- newIORef M.empty
  let machine = StackMachine [] 0 ver prefix transMap db (encodeTupleElems [Bytes prefix])
  runResourceT $ runMachine machine
