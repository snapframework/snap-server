{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Test.Blackbox
  ( tests
  , startTestServer ) where


import             Control.Concurrent
import             Control.Monad
import             Control.Monad.CatchIO
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.ByteString.Char8 as S
import qualified   Data.DList as D
import             Data.Int
import             Data.Maybe (fromJust)
import qualified   Network.HTTP as HTTP
import qualified   Network.URI as URI
import             Network.Socket
import qualified   Network.Socket.ByteString as N
import             Prelude hiding (take)
import             System.Timeout
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
tests port = map ($ port) [ testPong
                          , testHeadPong
                          , testEcho
                          , testRot13
                          , testSlowLoris
                          , testBlockingRead
                          , testBigResponse
                          , testPartial ]


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


doPong :: Int -> IO String
doPong port = do
    rsp <- HTTP.simpleHTTP $
           HTTP.getRequest $
           "http://localhost:" ++ show port ++ "/pong"

    HTTP.getResponseBody rsp


headPong :: Int -> IO String
headPong port = do
    let req = (HTTP.getRequest $ 
               "http://localhost:" ++ show port ++ "/pong")
                { HTTP.rqMethod = HTTP.HEAD }

    rsp <- HTTP.simpleHTTP req

    HTTP.getResponseBody rsp


testPong :: Int -> Test
testPong port = testCase "blackbox/pong" $ do
    doc <- doPong port
    assertEqual "pong response" "PONG" doc


testHeadPong :: Int -> Test
testHeadPong port = testCase "blackbox/pong/HEAD" $ do
    doc <- headPong port
    assertEqual "pong HEAD response" "" doc


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
testSlowLoris port = testCase "blackbox/slowloris" $ withSock port go

  where
    go sock = do
        m <- timeout (120*seconds) $ go' sock
        maybe (assertFailure "slowloris: timeout")
              (const $ return ())
              m

    go' sock = do
        N.sendAll sock "POST /echo HTTP/1.1\r\n"
        N.sendAll sock "Host: 127.0.0.1\r\n"
        N.sendAll sock "Content-Length: 2500000\r\n"
        N.sendAll sock "Connection: close\r\n\r\n"

        b <- expectExceptionBeforeTimeout (loris sock) 60

        assertBool "didn't catch slow loris attack" b

    loris sock = do
        N.sendAll sock "."
        waitabit
        loris sock


testBlockingRead :: Int -> Test
testBlockingRead port = testCase "blackbox/testBlockingRead" $
                        withSock port $ \sock -> do
    m <- timeout (60*seconds) $ go sock
    maybe (assertFailure "timeout")
          (const $ return ())
          m

  where
    go sock = do
        N.sendAll sock "GET /"
        waitabit
        N.sendAll sock "pong HTTP/1.1\r\n"
        N.sendAll sock "Host: 127.0.0.1\r\n"
        N.sendAll sock "Content-Length: 0\r\n"
        N.sendAll sock "Connection: close\r\n\r\n"

        resp <- recvAll sock

        let s = head $ ditchHeaders $ S.lines resp

        assertEqual "pong response" "PONG" s


-- test server's ability to trap/recover from IO errors
testPartial :: Int -> Test
testPartial port = testCase "blackbox/testPartial" $ do
    m <- timeout (60*seconds) go
    maybe (assertFailure "timeout")
          (const $ return ())
          m


  where
    go = do
        withSock port $ \sock ->
            N.sendAll sock "GET /pong HTTP/1.1\r\n"

        doc <- doPong port
        assertEqual "pong response" "PONG" doc


testBigResponse :: Int -> Test
testBigResponse port = testCase "blackbox/testBigResponse" $
                       withSock port $ \sock -> do
    m <- timeout (120*seconds) $ go sock
    maybe (assertFailure "timeout")
          (const $ return ())
          m
    
  where
    go sock = do
        N.sendAll sock "GET /bigresponse HTTP/1.1\r\n"
        N.sendAll sock "Host: 127.0.0.1\r\n"
        N.sendAll sock "Content-Length: 0\r\n"
        N.sendAll sock "Connection: close\r\n\r\n"

        let body = S.replicate 4000000 '.'
        resp <- recvAll sock

        let s = head $ ditchHeaders $ S.lines resp

        assertBool "big response" $ body == s


------------------------------------------------------------------------------
waitabit :: IO ()
waitabit = threadDelay $ 2*seconds


seconds :: Int
seconds = (10::Int) ^ (6::Int)
