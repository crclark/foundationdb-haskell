{-# LANGUAGE OverloadedStrings #-}

import FoundationDB.Internal.Bindings

main :: IO ()
main = withFoundationDB $ do
  let printerr err = getError err >>= print
  futureCluster <- createCluster ""
  err1 <- futureBlockUntilReady futureCluster
  printerr err1
  (err2, cluster) <- futureGetCluster futureCluster
  printerr err2
  futureDestroy futureCluster
  futureDB <- clusterCreateDatabase cluster
  err3 <- futureBlockUntilReady futureDB
  printerr err3
  (err4, db) <- futureGetDatabase futureDB
  printerr err4
  futureDestroy futureDB

  -- first transaction: set "foo" = "bar"
  (err5, trans) <- databaseCreateTransaction db
  printerr err5
  transactionSet trans "foo" "bar"
  futureCommit <- transactionCommit trans
  err9 <- futureBlockUntilReady futureCommit
  printerr err9
  futureDestroy futureCommit
  transactionDestroy trans

  -- second transaction: get "foo"
  (err6, trans2) <- databaseCreateTransaction db
  printerr err6
  futureGet <- transactionGet trans2 "foo" False
  err7 <- futureBlockUntilReady futureGet
  printerr err7
  (err8, mval) <- futureGetValue futureGet
  printerr err8
  print mval
  transactionDestroy trans2

  databaseDestroy db
  clusterDestroy cluster
