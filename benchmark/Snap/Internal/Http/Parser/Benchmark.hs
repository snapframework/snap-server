{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Parser.Benchmark
  ( benchmarks )
  where

import           Control.Monad
import           Criterion.Main                   hiding (run)
import qualified Data.ByteString                  as S
import           Snap.Internal.Http.Parser.Data
import           Snap.Internal.Http.Server.Parser
import qualified System.IO.Streams                as Streams

parseGet :: S.ByteString -> IO ()
parseGet s = do
    (Just !_) <- Streams.fromList [s] >>= parseRequest
    return $! ()


benchmarks :: Benchmark
benchmarks = bgroup "parser"
             [ bench "firefoxget" $ whnfIO $! replicateM_ 1000
                                           $! parseGet parseGetData
             ]
