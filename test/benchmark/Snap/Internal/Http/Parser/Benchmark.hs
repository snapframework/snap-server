{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Http.Parser.Benchmark
       ( benchmarks )
       where

import qualified Control.Exception as E
import           Control.Monad.Identity
import           Criterion.Main hiding (run)
import           Data.Attoparsec hiding (Result(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Snap.Internal.Http.Parser
import           Snap.Internal.Http.Parser.Data
import qualified Snap.Iteratee as SI
import           Snap.Iteratee hiding (take)

parseGet :: S.ByteString -> Identity ()
parseGet s = do
    !_ <- run_ $ enumBS s $$ parseRequest
    return ()


benchmarks = bgroup "parser"
             [ bench "firefoxget" $ whnf (runIdentity . parseGet) parseGetData
             ]
