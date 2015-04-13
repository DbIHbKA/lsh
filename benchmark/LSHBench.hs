-- benchmark/LSHBench
{-# LANGUAGE OverloadedStrings #-}
module LSHBench where

import           Criterion.Main
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char
import           Data.Foldable              (foldr')
import qualified Data.LSH                   as L
import           Prelude                    hiding (foldr)


setupTEnv = B.readFile "benchmark/data/1000.csv"
setupTTEnv = B.readFile "benchmark/data/2000.csv"
setupFTEnv = B.readFile "benchmark/data/5000.csv"
setupTenTEnv = B.readFile "benchmark/data/10000.csv"
setupTwentyTEnv = B.readFile "benchmark/data/20000.csv"
setupHTEnv = B.readFile "benchmark/data/100000.csv"
setupFullEnv = B.readFile "benchmark/data/FULL.csv"


add r = L.insert key value
          where key = head sr
                value = tail sr
                sr = C.split ',' r


search lsh r = L.nearest value lsh
    where sr = C.split ',' r
          value = tail sr

insert :: Int -> Int -> B.ByteString -> L.LSH B.ByteString
insert band row s = foldr'
        add
        (L.new band row)
        (C.lines s)

query :: Int -> Int -> B.ByteString -> [[B.ByteString]]
query band row s = take
        1
        (map
             (search lsh)
             (C.lines s))
    where lsh = insert band row s

benchmarks :: [Benchmark]
benchmarks = [ env setupFTEnv $
               \s ->
                    bgroup
                        "20000"
                        [ bench "query band=50 row=2" $
                          nf (query 50 2) s
                        , bench "query band=100 row=2" $
                          nf (query 100 2) s
                        , bench "query band=50 row=4" $
                          nf (query 50 4) s]
             , env setupTEnv $
               \s ->
                    bgroup
                        "1000"
                        [ bench "insert band=50 row=2" $
                          whnf (insert 50 2) s
                        , bench "insert band=100 row=2" $
                          whnf (insert 100 2) s
                        , bench "insert band=50 row=4" $
                          whnf (insert 50 4) s]
             , env setupTTEnv $
               \s ->
                    bgroup
                        "2000"
                        [ bench "insert band=50 row=2" $
                          whnf (insert 50 2) s
                        , bench "insert band=100 row=2" $
                          whnf (insert 100 2) s
                        , bench "insert band=50 row=4" $
                          whnf (insert 50 4) s]
             , env setupFTEnv $
               \s ->
                    bgroup
                        "5000"
                        [ bench "insert band=50 row=2" $
                          whnf (insert 50 2) s
                        , bench "insert band=100 row=2" $
                          whnf (insert 100 2) s
                        , bench "insert band=50 row=4" $
                          whnf (insert 50 4) s]
             , env setupTenTEnv $
               \s ->
                    bgroup
                        "10000"
                        [ bench "insert band=50 row=2" $
                          whnf (insert 50 2) s
                        , bench "insert band=100 row=2" $
                          whnf (insert 100 2) s
                        , bench "insert band=50 row=4" $
                          whnf (insert 50 4) s]
             , env setupTwentyTEnv $
               \s ->
                    bgroup
                        "20000"
                        [ bench "insert band=50 row=2" $
                          whnf (insert 50 2) s
                        , bench "insert band=100 row=2" $
                          whnf (insert 100 2) s
                        , bench "insert band=50 row=4" $
                          whnf (insert 50 4) s]
             , env setupHTEnv $
               \s ->
                    bgroup
                        "100000"
                        [ bench "insert band=50 row=2" $
                          whnf (insert 50 2) s
                        , bench "insert band=100 row=2" $
                          whnf (insert 100 2) s
                        , bench "insert band=50 row=4" $
                          whnf (insert 50 4) s]]
