{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent

import           Snap.Iteratee
import           Snap.Types
import           Snap.Http.Server
import Snap.Util.GZip
-- FIXME: need better primitives for output
pongServer :: Snap ()
pongServer = withCompression $
             modifyResponse $ setResponseBody (enumBS "PONG") .
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
        httpServe "*" 3000 "localhost" (Just "foo.log") Nothing pongServer 
        --httpServe "*" 3000 "localhost" Nothing Nothing pongServer 
        putMVar m ()
