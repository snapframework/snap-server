{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Common.TestHandler (testHandler) where


import           Control.Monad

import qualified Data.ByteString.Char8 as B
import           Data.Maybe

import           Snap.Iteratee hiding (Enumerator)
import           Snap.Types
import           Snap.Http.Server
import           Snap.Util.FileServe

import           Snap.Internal.Iteratee.Debug


pongHandler :: Snap ()
pongHandler = modifyResponse $ setResponseBody (enumBS "PONG") .
                               setContentType "text/plain" .
                               setContentLength 4

echoUriHandler :: Snap ()
echoUriHandler = do
    req <- getRequest
    writeBS $ rqURI req


echoHandler :: Snap ()
echoHandler = transformRequestBody return


responseHandler :: Snap ()
responseHandler = do
    !code <- liftM (read . B.unpack . fromMaybe "503") $ getParam "code"
    modifyResponse $ setResponseCode code
    writeBS $ B.pack $ show code


testHandler :: Snap ()
testHandler =
    route [ ("pong", pongHandler)
          , ("echo", echoHandler)
          , ("echoUri", echoUriHandler)
          , ("fileserve", fileServe "testserver/static")
          , ("respcode/:code", responseHandler)
          ]

