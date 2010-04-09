{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent

import           Snap.Iteratee
import           Snap.Types
import           Snap.Http.Server

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
    go m = do
        httpServe "*" 8000 "localhost" Nothing Nothing pongServer 
        putMVar m ()
