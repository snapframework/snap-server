{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Blaze.ByteString.Builder
import           Control.Concurrent
import           Control.Exception (finally)

import           Snap.Iteratee
import           Snap.Types
import           Snap.Http.Server

-- FIXME: need better primitives for output
pongServer :: Snap ()
pongServer = modifyResponse $ setResponseBody enum .
                              setContentType "text/plain" .
                              setContentLength 4
  where
    enum = enumBuilder $ fromByteString "PONG"

main :: IO ()
main = do
    m <- newEmptyMVar

    forkIO $ go m
    takeMVar m

    return ()

  where
    go m   = httpServe config pongServer `finally` putMVar m ()
    config = setPort 8000 $
             setErrorLog Nothing $
             setAccessLog Nothing $
             setCompression False $ emptyConfig
