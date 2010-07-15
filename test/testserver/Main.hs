{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent

import           Snap.Iteratee
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

echoHandler :: Snap ()
echoHandler = do
    req <- getRequest
    writeBS $ rqPathInfo req

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

