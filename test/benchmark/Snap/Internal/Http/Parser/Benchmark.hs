{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Http.Parser.Benchmark
       ( benchmarks )
       where

import qualified   Control.Exception as E
import "monads-fd" Control.Monad.Identity
import             Criterion.Main hiding (run)
import             Data.Attoparsec hiding (Result(..))
import             Data.ByteString (ByteString)
import qualified   Data.ByteString as S
import qualified   Data.ByteString.Lazy.Char8 as L
import             Snap.Internal.Http.Parser
import             Snap.Internal.Http.Parser.Data
import qualified   Snap.Iteratee as SI
import             Snap.Iteratee hiding (take)

parseGet ::  IO ()
parseGet = do
    step <- runIteratee parseRequest
    run_ $ enumBS parseGetData step
    return ()


parseChunked :: IO ()
parseChunked = do
    sstep <- runIteratee stream2stream
    c     <- toChunked parseChunkedData
    cstep <- runIteratee $ readChunkedTransferEncoding sstep
    let i  = enumBS c cstep
    f     <- run_ i
    return ()

-- utils
toChunked :: L.ByteString -> IO ByteString
toChunked lbs = do
    sstep <- runIteratee stream2stream
    cstep <- runIteratee $ joinI $ writeChunkedTransferEncoding sstep
    run_ $ enumLBS lbs cstep

benchmarks = bgroup "parser"
             [ bench "firefoxget" $ whnfIO parseGet
             , bench "readChunkedTransferEncoding" $ whnfIO parseChunked ]


stream2stream :: (Monad m) => Iteratee ByteString m ByteString              
stream2stream = liftM S.concat consume                
