module Main where

import FoundationDB.Layer.Tuple.Internal

import Gauge.Main

main :: IO ()
main =
  Gauge.Main.defaultMain
  [ bgroup "bisectSize"
    [ bench "bisectSize small" (nf bisectSize 1025)
    , bench "bisectSize medium" (nf bisectSize 123456789012)
    , bench "bisectSize large"  (nf bisectSize 1234567890123454555)]]
