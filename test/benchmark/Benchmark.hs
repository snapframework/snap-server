module Main where

import           Criterion.Main
import qualified Snap.Internal.Http.Parser.Benchmark as PB

main :: IO ()
main = defaultMain [ PB.benchmarks ]
