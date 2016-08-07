{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Parser.Benchmark
  ( benchmarks )
  where

import           Control.Monad
import qualified Criterion.Main                   as C
import qualified Data.ByteString                  as S
import           Snap.Internal.Http.Parser.Data
import           Snap.Internal.Http.Server.Parser
import qualified System.IO.Streams                as Streams

parseGet :: S.ByteString -> IO ()
parseGet s = do
    !_ <- Streams.fromList [s] >>= parseRequest
    return $! ()


benchmarks :: C.Benchmark
benchmarks = C.bgroup "parser"
             [ C.bench "firefoxget" $ C.whnfIO $! replicateM_ 1000
                                               $! parseGet parseGetData
             ]
