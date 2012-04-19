{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Blaze.ByteString.Builder
import           Control.Concurrent
import           Control.Exception (finally)

import           Snap.Iteratee
import           Snap.Core
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
    m      <- newEmptyMVar
    config <- commandLineConfig defaults
    forkIO $ go m config
    takeMVar m
    return ()

  where
    defaults    = setPort 8000 $
                  setErrorLog ConfigNoLog $
                  setAccessLog ConfigNoLog $
                  setCompression False $
                  setVerbose False $
                  emptyConfig

    go m config = httpServe config pongServer `finally` putMVar m ()
