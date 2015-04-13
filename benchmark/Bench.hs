-- benchmark/Bench.hs
module Main where

import Criterion.Main (bgroup, defaultMain)
import qualified LSHBench


main :: IO ()
main = defaultMain
        [bgroup "LSH" LSHBench.benchmarks]
