{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Test.Blackbox
  ( tests
  , startTestServer ) where


import             Control.Concurrent
import             Control.Monad
import             Control.Monad.CatchIO
import qualified   Data.ByteString as S
import             Data.Int
import             Data.Maybe (fromJust)
import qualified   Network.HTTP as HTTP
import qualified   Network.URI as URI
import             Network.Socket
import qualified   Network.Socket.ByteString as N
import             Prelude hiding (take)
import             Test.Framework
import             Test.Framework.Providers.HUnit
import             Test.Framework.Providers.QuickCheck2
import             Test.HUnit hiding (Test, path)
import             Test.QuickCheck
import qualified   Test.QuickCheck.Monadic as QC
import             Test.QuickCheck.Monadic hiding (run, assert)

import             Snap.Http.Server
import             Snap.Test.Common

import             Test.Common.Rot13
import             Test.Common.TestHandler


tests :: Int -> [Test]
tests port = [ testPong      port
             , testEcho      port
             , testRot13     port
             , testSlowLoris port ]


startTestServer :: IO (ThreadId,Int)
startTestServer = do
    tid <- forkIO $
           httpServe "*"
                     port
                     "localhost"
                     (Just "ts-access.log")
                     (Just "ts-error.log")
                     testHandler
    waitabit

    return $ (tid, port)

  where
    port = 8199


testPong :: Int -> Test
testPong port = testCase "blackbox/pong" $ do
    rsp <- HTTP.simpleHTTP $
           HTTP.getRequest $
           "http://localhost:" ++ show port ++ "/pong"

    doc <- HTTP.getResponseBody rsp
    assertEqual "pong response" "PONG" doc


testEcho :: Int -> Test
testEcho port = testProperty "blackbox/echo" $
                monadicIO $ forAllM arbitrary prop
  where
    prop txt = do
        let uri = fromJust $
                  URI.parseURI $
                  "http://localhost:" ++ show port ++ "/echo"

        let len = S.length txt

        let req' = (HTTP.mkRequest HTTP.POST uri) :: HTTP.Request S.ByteString
        let req = HTTP.replaceHeader HTTP.HdrContentLength (show len) req'
                  
        rsp <- QC.run $ HTTP.simpleHTTP $ req { HTTP.rqBody = (txt::S.ByteString) }
        doc <- QC.run $ HTTP.getResponseBody rsp

        QC.assert $ txt == doc


testRot13 :: Int -> Test
testRot13 port = testProperty "blackbox/rot13" $
                 monadicIO $ forAllM arbitrary prop
  where
    prop txt = do
        let uri = fromJust $
                  URI.parseURI $
                  "http://localhost:" ++ show port ++ "/rot13"

        let len = S.length txt

        let req' = (HTTP.mkRequest HTTP.POST uri) :: HTTP.Request S.ByteString
        let req = HTTP.replaceHeader HTTP.HdrContentLength (show len) req'
                  
        rsp <- QC.run $ HTTP.simpleHTTP $ req { HTTP.rqBody = (txt::S.ByteString) }
        doc <- QC.run $ HTTP.getResponseBody rsp

        QC.assert $ txt == rot13 doc


testSlowLoris :: Int -> Test
testSlowLoris port = testCase "blackbox/slowloris" $ do
    addr <- liftM (addrAddress . Prelude.head) $
            getAddrInfo (Just myHints)
                        (Just "127.0.0.1")
                        (Just $ show port)

    sock <- socket AF_INET Stream defaultProtocol
    connect sock addr

    go sock `finally` sClose sock

  where
    myHints = defaultHints { addrFlags = [ AI_NUMERICHOST ] }

    go sock = do
        N.sendAll sock "POST /echo HTTP/1.1\r\n"
        N.sendAll sock "Host: 127.0.0.1\r\n"
        N.sendAll sock "Content-Length: 2500000\r\n"
        N.sendAll sock "Connection: close\r\n\r\n"

        b <- expectExceptionBeforeTimeout (loris sock) 60

        assertBool "didn't catch slow loris attack" b

    loris sock = do
        N.sendAll sock "."
        threadDelay 2000000
        loris sock


------------------------------------------------------------------------------
waitabit :: IO ()
waitabit = threadDelay $ 2*((10::Int)^(6::Int))
