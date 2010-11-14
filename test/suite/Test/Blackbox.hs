{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Test.Blackbox
  ( tests
  , spawnStunnel
  , killStunnel
  , ssltests
  , startTestServer ) where


import             Control.Concurrent
import             Control.Monad
import qualified   Data.ByteString.Char8 as S
import             Data.Int
import             Data.Maybe (fromJust)
import qualified   Network.HTTP as HTTP
import qualified   Network.URI as URI
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
import             System.Directory (getCurrentDirectory)
import             System.Posix.Signals (signalProcess, sigINT)
import             System.Posix.Types (ProcessID)
import             System.Process (runCommand)

import             Snap.Http.Server
import             Snap.Test.Common

import             Test.Common.Rot13
import             Test.Common.TestHandler

testFunctions :: [Int -> Test]
testFunctions = [ testPong
                , testHeadPong
                , testEcho
                , testRot13
                , testSlowLoris
                , testBlockingRead
                , testBigResponse
                , testPartial
                ]


tests :: Int -> [Test]
tests port = map ($ port) testFunctions


ssltests :: Maybe (Int,Int) -> [Test]
ssltests = maybe [] httpsTests
    where httpsTests (_,port) = map ($ port) testFunctions


startTestServer :: Int -> Maybe (Int,Int) -> ConfigBackend -> IO ThreadId
startTestServer port sslport backend = do
    let cfg = setAccessLog (Just $ "ts-access.log." ++ show backend)  .
              setErrorLog  (Just $ "ts-error.log." ++ show backend)   . 
              addListen    (ListenHttp "*" port)                      .
              setBackend   backend                                    .
              setVerbose   False                                      $
              defaultConfig

    let cfg' = case sslport of
                Nothing    -> cfg
                Just (p,_) -> addListen (ListenHttps "*" p "cert.pem" "key.pem") cfg
              
    tid <- forkIO $
           httpServe cfg' testHandler
    waitabit

    return tid

{- stunnel needs the SIGINT signal to properly shutdown, but
   System.Process can currently only send SIGKILL.  A future
   version of System.Process will have the ability to send SIGINT
   (http://hackage.haskell.org/trac/ghc/ticket/3994)
   so perhaps in the future we can simplify the code below. -}
spawnStunnel :: Maybe (Int, Int) -> IO (Maybe ProcessID)
spawnStunnel Nothing = return Nothing
spawnStunnel (Just (sport, lport)) = do
    tdir <- getCurrentDirectory
    let pidfile = tdir ++ "/snap.stunnel.pid"
    runCommand $ "stunnel -f -P " ++ pidfile ++
                        " -D 0 -c -d " ++ show lport ++
                        " -r " ++ show sport 

    waitabit
    str <- readFile pidfile
    return $ Just $ read str

killStunnel :: Maybe ProcessID -> IO ()
killStunnel Nothing = return ()
killStunnel (Just pid) = signalProcess sigINT pid 

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
