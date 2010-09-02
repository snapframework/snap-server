{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Concurrent

import           Snap.Iteratee hiding (Enumerator)
import           Snap.Types
import           Snap.Http.Server
import           Snap.Util.FileServe

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
    writeBS $ rqPathInfo req


echoHandler :: Snap ()
echoHandler = do
    unsafeDetachRequestBody >>= \e -> do
      let (SomeEnumerator x) = e
      modifyResponse $ setResponseBody x


responseHandler = do
    code <- getParam "code"
    case code of
        Nothing   -> undefined
        Just code -> f code
  where
    f "300" = undefined
    f "304" = undefined


handlers :: Snap ()
handlers =
    route [ ("pong", pongHandler)
          , ("echo", echoHandler)
          , ("echoUri", echoUriHandler)
          , ("fileserve", fileServe "static")
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
        httpServe "*" 3000 "localhost" Nothing Nothing handlers 
        putMVar m ()

