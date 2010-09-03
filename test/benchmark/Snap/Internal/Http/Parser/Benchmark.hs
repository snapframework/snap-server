{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Http.Parser.Benchmark 
       ( benchmarks )
       where

import             Criterion.Main hiding (run)
import             Snap.Internal.Http.Parser
import             Data.ByteString (ByteString)
import qualified   Data.ByteString as S
import qualified   Snap.Iteratee as SI
import qualified   Control.Exception as E
import             Data.Attoparsec hiding (Result(..))
import             Snap.Internal.Http.Parser.Data
import "monads-fd" Control.Monad.Identity
import             Data.Iteratee
import             Data.Iteratee.WrappedByteString
import             Snap.Iteratee hiding (take, foldl', filter)
import qualified Data.ByteString.Lazy.Char8 as L

parseGet ::  IO ()
parseGet = SI.enumBS parseGetData parseRequest >>= SI.run >> return ()

parseChunked :: IO ()
parseChunked = do
  c <- toChunked parseChunkedData
  i <- SI.enumLBS c (readChunkedTransferEncoding stream2stream)
  f <- SI.run i
  return ()

-- utils
toChunked lbs = writeChunkedTransferEncoding (enumLBS lbs) stream2stream >>= run >>= return . fromWrap

benchmarks = bgroup "parser"
             [ bench "firefoxget" $ whnfIO parseGet
             , bench "readChunkedTransferEncoding" $ whnfIO parseChunked ]
