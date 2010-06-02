module Main where

import Criterion.Main

import qualified Snap.Internal.Http.Parser.Benchmark as PB

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = defaultMain [
	PB.benchmarks
       ]
