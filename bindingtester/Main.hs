{-# LANGUAGE LambdaCase #-}

module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import System.Environment

import StackMachine

import FoundationDB

go :: ByteString -> Int -> Maybe FilePath -> IO ()
go prefix ver mpath =
  withFoundationDB (FoundationDBOptions ver mpath [] []) $ \ database ->
    runTests ver prefix database

main :: IO ()
main = do
  args <- getArgs
  case args of
    [prefixStr, verStr, pathStr] ->
      go (pack prefixStr) (read verStr) (Just pathStr)
    [prefixStr, verStr] ->
      go (pack prefixStr) (read verStr) Nothing
    _ -> error $ "unexpected args: " ++ show args
