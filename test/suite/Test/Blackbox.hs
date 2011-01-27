{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Test.Blackbox
  ( tests
  , ssltests
  , startTestServer ) where

------------------------------------------------------------------------------
import             Control.Concurrent
import             Control.Exception (SomeException, catch)
import             Control.Monad
import qualified   Data.ByteString.Char8 as S
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.ByteString.Lazy.Char8 as L
import             Data.Int
import qualified   Network.HTTP.Enumerator as HTTP
import qualified   Network.Socket.ByteString as N
import             Prelude hiding (catch, take)
import             System.Timeout
import             Test.Framework
import             Test.Framework.Providers.HUnit
import             Test.Framework.Providers.QuickCheck2
import             Test.HUnit hiding (Test, path)
import             Test.QuickCheck
import qualified   Test.QuickCheck.Monadic as QC
import             Test.QuickCheck.Monadic hiding (run, assert)
------------------------------------------------------------------------------
import             Snap.Http.Server
import             Snap.Test.Common
import             Test.Common.Rot13
import             Test.Common.TestHandler
------------------------------------------------------------------------------

testFunctions :: [Bool -> Int -> String -> Test]
testFunctions = [ testPong
-- FIXME: waiting on http-enumerator patch for HEAD behaviour
--                , testHeadPong
                , testEcho
                , testRot13
                , testSlowLoris
                , testBlockingRead
                , testBigResponse
                , testPartial
                ]


------------------------------------------------------------------------------
tests :: Int -> String -> [Test]
tests port name = map (\f -> f False port name) testFunctions


------------------------------------------------------------------------------
ssltests :: String -> Maybe Int -> [Test]
ssltests name = maybe [] httpsTests
    where httpsTests port = map (\f -> f True port sslname) testFunctions
          sslname = "ssl/" ++ name

------------------------------------------------------------------------------
startTestServer :: Int
                -> Maybe Int
                -> ConfigBackend
                -> IO (ThreadId, MVar ())
startTestServer port sslport backend = do
    let cfg = setAccessLog (Just $ "ts-access." ++ show backend ++ ".log") .
              setErrorLog  (Just $ "ts-error." ++ show backend ++ ".log")  .
              addListen    (ListenHttp "*" port)                           .
              setBackend   backend                                         .
              setDefaultTimeout 10                                         .
              setVerbose   False                                           $
              defaultConfig

    let cfg' = case sslport of
                Nothing -> cfg
                Just p  -> addListen
                           (ListenHttps "*" p "cert.pem" "key.pem")
                           cfg

    mvar <- newEmptyMVar
    tid  <- forkIO $ do
                (httpServe cfg' testHandler)
                  `catch` \(_::SomeException) -> return ()
                putMVar mvar ()
    waitabit

    return (tid,mvar)


------------------------------------------------------------------------------
doPong :: Bool -> Int -> IO ByteString
doPong ssl port = do
    let uri = (if ssl then "https" else "http")
              ++ "://127.0.0.1:" ++ show port ++ "/pong"

    rsp <- HTTP.simpleHttp uri
    return $ S.concat $ L.toChunks rsp


------------------------------------------------------------------------------
-- FIXME: waiting on http-enumerator patch for HEAD behaviour
-- headPong :: Bool -> Int -> IO ByteString
-- headPong ssl port = do
--     let uri = (if ssl then "https" else "http")
--               ++ "://127.0.0.1:" ++ show port ++ "/echo"

--     req0 <- HTTP.parseUrl uri

--     let req = req0 { HTTP.method = "HEAD" }
--     rsp <- HTTP.httpLbs req
--     return $ S.concat $ L.toChunks $ HTTP.responseBody rsp

------------------------------------------------------------------------------
testPong :: Bool -> Int -> String -> Test
testPong ssl port name = testCase (name ++ "/blackbox/pong") $ do
    doc <- doPong ssl port
    assertEqual "pong response" "PONG" doc


------------------------------------------------------------------------------
-- FIXME: waiting on http-enumerator patch for HEAD behaviour
-- testHeadPong :: Bool -> Int -> String -> Test
-- testHeadPong ssl port name = testCase (name ++ "/blackbox/pong/HEAD") $ do
--     doc <- headPong ssl port
--     assertEqual "pong HEAD response" "" doc


------------------------------------------------------------------------------
testEcho :: Bool -> Int -> String -> Test
testEcho ssl port name = testProperty (name ++ "/blackbox/echo") $
                         monadicIO $ forAllM arbitrary prop
  where
    prop txt = do
        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/echo"

        req0 <- QC.run $ HTTP.parseUrl uri
        let req = req0 { HTTP.requestBody = txt
                       , HTTP.method = "POST" }

        rsp <- QC.run $ HTTP.httpLbs req
        let doc = HTTP.responseBody rsp

        QC.assert $ txt == doc


------------------------------------------------------------------------------
testRot13 :: Bool -> Int -> String -> Test
testRot13 ssl port name = testProperty (name ++ "/blackbox/rot13") $
                          monadicIO $ forAllM arbitrary prop
  where
    prop txt = do
        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/rot13"

        req0 <- QC.run $ HTTP.parseUrl uri
        let req = req0 { HTTP.requestBody = L.fromChunks [txt]
                       , HTTP.method = "POST" }

        rsp <- QC.run $ HTTP.httpLbs req
        let doc = S.concat $ L.toChunks $ HTTP.responseBody rsp

        QC.assert $ txt == rot13 doc


------------------------------------------------------------------------------
-- TODO: this one doesn't work w/ SSL
testSlowLoris :: Bool -> Int -> String -> Test
testSlowLoris ssl port name = testCase (name ++ "/blackbox/slowloris") $
                              if ssl then return () else withSock port go

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


------------------------------------------------------------------------------
-- TODO: doesn't work w/ ssl
testBlockingRead :: Bool -> Int -> String -> Test
testBlockingRead ssl port name =
    testCase (name ++ "/blackbox/testBlockingRead") $
             if ssl then return () else runIt

  where
    runIt = withSock port $ \sock -> do
        m <- timeout (60*seconds) $ go sock
        maybe (assertFailure "timeout")
              (const $ return ())
              m

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


------------------------------------------------------------------------------
-- TODO: no ssl here
-- test server's ability to trap/recover from IO errors
testPartial :: Bool -> Int -> String -> Test
testPartial ssl port name =
    testCase (name ++ "/blackbox/testPartial") $
    if ssl then return () else runIt

  where
    runIt = do
        m <- timeout (60*seconds) go
        maybe (assertFailure "timeout")
              (const $ return ())
              m

    go = do
        withSock port $ \sock ->
            N.sendAll sock "GET /pong HTTP/1.1\r\n"

        doc <- doPong ssl port
        assertEqual "pong response" "PONG" doc


------------------------------------------------------------------------------
-- TODO: no ssl
testBigResponse :: Bool -> Int -> String -> Test
testBigResponse ssl port name =
    testCase (name ++ "/blackbox/testBigResponse") $
    if ssl then return () else runIt
  where
    runIt = withSock port $ \sock -> do
        m <- timeout (120*seconds) $ go sock
        maybe (assertFailure "timeout")
              (const $ return ())
              m

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


------------------------------------------------------------------------------
seconds :: Int
seconds = (10::Int) ^ (6::Int)
