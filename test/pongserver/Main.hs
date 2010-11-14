{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Exception (finally)

import           Snap.Iteratee
import           Snap.Types
import           Snap.Http.Server
import Snap.Util.GZip

-- FIXME: need better primitives for output
pongServer :: Snap ()
pongServer = modifyResponse $ setResponseBody (enumBS "PONG") .
                              setContentType "text/plain" .
                              setContentLength 4


main :: IO ()
main = do
    m <- newEmptyMVar

    forkIO $ go m
    takeMVar m

    return ()

  where
    go m   = httpServe config pongServer `finally` putMVar m ()
    config = addListen (ListenHttp "*" 8000) $
             setErrorLog Nothing $
             setAccessLog Nothing $ 
             setCompression False $ emptyConfig
