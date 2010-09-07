{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Concurrent
import           Control.Monad

import qualified Data.ByteString.Char8 as B
import           Data.Maybe

import           Snap.Iteratee hiding (Enumerator)
import           Snap.Types
import           Snap.Http.Server
import           Snap.Util.FileServe


import Snap.Internal.Iteratee.Debug

{-

/pong
/fileserve
/echo
pipelined POST requests
slowloris attack / timeout test

-}

pongHandler :: Snap ()
pongHandler = modifyResponse $ setResponseBody (enumBS "PONG") .
                               setContentType "text/plain" .
                               setContentLength 4

echoUriHandler :: Snap ()
echoUriHandler = do
    req <- getRequest
    writeBS $ rqURI req


echoHandler :: Snap ()
echoHandler = do
    unsafeDetachRequestBody >>= \e -> do
      let (SomeEnumerator x) = e
      let e' i = x (iterateeDebugWrapper "echoHandler" i)
      modifyResponse $ setResponseBody e'


responseHandler :: Snap ()
responseHandler = do
    !code <- liftM (read . B.unpack . fromMaybe "503") $ getParam "code"
    modifyResponse $ setResponseCode code
    writeBS $ B.pack $ show code


handlers :: Snap ()
handlers =
    route [ ("pong", pongHandler)
          , ("echo", echoHandler)
          , ("echoUri", echoUriHandler)
          , ("fileserve", fileServe "testserver/static")
          , ("respcode/:code", responseHandler)
          ]


main :: IO ()
main = do
    m <- newEmptyMVar

    forkIO $ go m
    takeMVar m

    return ()

  where
    go m = do
        httpServe "*" 3000 "localhost" (Just "ts-access.log")
                  (Just "ts-error.log") handlers 
        putMVar m ()

