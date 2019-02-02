{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)

import FoundationDB.Versionstamp
import FoundationDB.Layer.Tuple.Internal

import Gauge.Main

mixedTuple :: ByteString
mixedTuple = "\SOHsome_prefix\NUL\SOHsome_other_prefix\NUL3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\STX\NUL\ETX\NAK\f"

main :: IO ()
main =
  Gauge.Main.defaultMain
  [ bgroup "bisectSize"
    [ bench "bisectSize small" (nf bisectSize 1025)
    , bench "bisectSize medium" (nf bisectSize 123456789012)
    , bench "bisectSize large"  (nf bisectSize 1234567890123454555)]

  , bgroup "encodeTupleElems"
    [ bench "Tuple" (nf encodeTupleElems [Tuple [Bytes "hello"]])
    , bench "Bytes" (nf encodeTupleElems [Bytes "hello"])
    , bench "Text"  (nf encodeTupleElems [Text "hello"])
    , bench "Int small pos" (nf encodeTupleElems [Int 1024])
    , bench "Int small neg" (nf encodeTupleElems [Int (-1024)])
    , bench "Int large pos" (nf encodeTupleElems [Int 102400000000000000])
    , bench "Int large neg" (nf encodeTupleElems [Int (-102400000000000000)])
    , bench "Float" (nf encodeTupleElems [Float 12.356])
    , bench "Double" (nf encodeTupleElems [Double 12.356])
    , bench "Bool" (nf encodeTupleElems [Bool True])
    , bench "UUID" (nf encodeTupleElems [UUID 1 2 3 4])
    , bench "CompleteVS"
            (nf encodeTupleElems
                [CompleteVS (CompleteVersionstamp
                              (TransactionVersionstamp 1 2)
                              3)])
    , bench "IncompleteVS"
            (nf encodeTupleElems [IncompleteVS (IncompleteVersionstamp 1)])
    , bench "Mixed"
            (nf encodeTupleElems
                [ Bytes "some_prefix"
                , Bytes "some_other_prefix"
                , IncompleteVS (IncompleteVersionstamp 123)
                , Int 12])
    ]

  , bgroup "decodeTupleElems"
    [ bench "Tuple" (nf decodeTupleElems "\ENQ\SOHhello\NUL\NUL")
    , bench "Bytes" (nf decodeTupleElems "\SOHhello\NUL")
    , bench "Text"  (nf decodeTupleElems "\STXhello\NUL")
    , bench "Int small pos" (nf decodeTupleElems "\SYN\EOT\NUL")
    , bench "Int small neg" (nf decodeTupleElems "\DC2\251\255")
    , bench "Int large pos" (nf decodeTupleElems "\FS\SOHk\204A\233\NUL\NUL\NUL")
    , bench "Int large neg" (nf decodeTupleElems "\f\254\148\&3\190\SYN\255\255\255")
    , bench "Float" (nf decodeTupleElems " \193E\178-")
    , bench "Double" (nf decodeTupleElems "!\192(\182E\161\202\192\131")
    , bench "Bool" (nf decodeTupleElems "'")
    , bench "UUID" (nf decodeTupleElems "0\NUL\NUL\NUL\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL\ETX\NUL\NUL\NUL\EOT")
    , bench "CompleteVS" (nf decodeTupleElems "3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\STX\NUL\ETX")
    , bench "Mixed" (nf decodeTupleElems mixedTuple)
    , bench "WPrefix" (nf (decodeTupleElemsWPrefix
                            "\SOHsome_prefix\NUL\SOHsome_other_prefix\NUL")
                            mixedTuple)
    ]
  ]
