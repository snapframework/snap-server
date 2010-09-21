{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Common.TestHandler (testHandler) where


import           Control.Monad

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Iteratee.WrappedByteString
import           Data.Maybe

import           Snap.Iteratee hiding (Enumerator)
import           Snap.Types
import           Snap.Http.Server
import           Snap.Util.FileServe
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
echoHandler = transformRequestBody return


rot13Handler :: Snap ()
rot13Handler = transformRequestBody $ return . f
  where
    f i    = IterateeG $ \ch -> do
                 case ch of
                   (EOF _)            -> runIter i ch
                   (Chunk (WrapBS s)) -> do
                        i' <- liftM liftI $ runIter i $ Chunk $ WrapBS $ rot13 s
                        return $ Cont (f i') Nothing


bigResponseHandler :: Snap ()
bigResponseHandler = do
    let sz = 4000000
    let s = L.take sz $ L.cycle $ L.replicate 4096 '.'
    modifyResponse $ setContentLength sz
    writeLBS s


responseHandler :: Snap ()
responseHandler = do
    !code <- liftM (read . B.unpack . fromMaybe "503") $ getParam "code"
    modifyResponse $ setResponseCode code
    writeBS $ B.pack $ show code


testHandler :: Snap ()
testHandler =
    route [ ("pong"           , pongHandler                  )
          , ("echo"           , echoHandler                  )
          , ("rot13"          , rot13Handler                 )
          , ("echoUri"        , echoUriHandler               )
          , ("fileserve"      , fileServe "testserver/static")
          , ("bigresponse"    , bigResponseHandler           )
          , ("respcode/:code" , responseHandler              )
          ]

