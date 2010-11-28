{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Common.TestHandler (testHandler) where


import           Control.Monad
import           Control.Monad.Trans

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Maybe

import           Snap.Iteratee hiding (Enumerator)
import qualified Snap.Iteratee as I
import           Snap.Types
import           Snap.Http.Server
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           Snap.Internal.Iteratee.Debug
import           Test.Common.Rot13 (rot13)


pongHandler :: Snap ()
pongHandler = modifyResponse $ setResponseBody (enumBS "PONG") .
                               setContentType "text/plain" .
                               setContentLength 4

echoUriHandler :: Snap ()
echoUriHandler = do
    req <- getRequest
    writeBS $ rqURI req


echoHandler :: Snap ()
echoHandler = transformRequestBody returnI


rot13Handler :: Snap ()
rot13Handler = transformRequestBody f
  where
    f origStep = do
        mbX  <- I.head
        maybe (enumEOF origStep)
              (feedStep origStep)
              mbX

    feedStep origStep x = do
        step <- lift $ runIteratee $ enumBS (rot13 x) origStep
        f step


bigResponseHandler :: Snap ()
bigResponseHandler = do
    let sz = 4000000
    let s = L.take sz $ L.cycle $ L.fromChunks [S.replicate 400000 '.']
    modifyResponse $ setContentLength sz
    writeLBS s


responseHandler :: Snap ()
responseHandler = do
    !code <- liftM (read . S.unpack . fromMaybe "503") $ getParam "code"
    modifyResponse $ setResponseCode code
    writeBS $ S.pack $ show code


testHandler :: Snap ()
testHandler = withCompression $
    route [ ("pong"           , pongHandler                  )
          , ("echo"           , echoHandler                  )
          , ("rot13"          , rot13Handler                 )
          , ("echoUri"        , echoUriHandler               )
          , ("fileserve"      , fileServe "testserver/static")
          , ("bigresponse"    , bigResponseHandler           )
          , ("respcode/:code" , responseHandler              )
          ]

