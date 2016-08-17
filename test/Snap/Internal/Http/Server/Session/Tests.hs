{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.Session.Tests (tests) where

------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,6,0)
import           Prelude                           hiding (catch)
#endif
import           Control.Concurrent                (MVar, forkIO, killThread, modifyMVar_, myThreadId, newChan, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, threadDelay, throwTo, withMVar)
import           Control.Exception.Lifted          (AsyncException (ThreadKilled), Exception, SomeException (..), bracket, catch, evaluate, mask, throwIO, try)
import           Control.Monad                     (forM_, liftM, replicateM_, void, when, (>=>))
import           Control.Monad.State.Class         (modify)
import           Data.ByteString.Builder           (Builder, byteString, char8, toLazyByteString)
import           Data.ByteString.Builder.Extra     (flush)
import           Data.ByteString.Builder.Internal  (newBuffer)
import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Char8             as S
import qualified Data.ByteString.Lazy.Char8        as L
import qualified Data.CaseInsensitive              as CI
import           Data.IORef                        (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map                          as Map
import           Data.Maybe                        (isNothing)
#if !MIN_VERSION_base(4,6,0)
import           Data.Monoid                       (mappend)
#endif
import           Data.Time.Clock.POSIX             (posixSecondsToUTCTime)
import           Data.Typeable                     (Typeable)
import           Data.Word                         (Word64)
import qualified Network.Http.Client               as Http
import           System.IO.Streams                 (InputStream, OutputStream, makeOutputStream)
import qualified System.IO.Streams                 as Streams
import qualified System.IO.Streams.Concurrent      as Streams
import qualified System.IO.Streams.Debug           as Streams
import           System.Timeout                    (timeout)
import           Test.Framework                    (Test, testGroup)
import           Test.Framework.Providers.HUnit    (testCase)
import qualified Test.Framework.Runners.Console    as Console
import           Test.HUnit                        (assertBool, assertEqual)
------------------------------------------------------------------------------
import           Snap.Core                         (Cookie (Cookie, cookieName, cookieValue), Request (rqContentLength, rqCookies, rqHostName, rqLocalHostname, rqPathInfo, rqQueryString, rqURI), Snap, addResponseCookie, escapeHttp, getHeader, getRequest, getsRequest, modifyResponse, readRequestBody, rqParam, rqPostParam, rqQueryParam, sendFile, sendFilePartial, setContentLength, setHeader, setResponseBody, setResponseStatus, terminateConnection, writeBS, writeBuilder, writeLBS)
import           Snap.Http.Server.Types            (emptyServerConfig, getDefaultTimeout, getIsSecure, getLocalAddress, getLocalHostname, getLocalPort, getLogAccess, getLogError, getNumAcceptLoops, getOnDataFinished, getOnEscape, getOnException, getOnNewRequest, getOnParse, getOnUserHandlerFinished, getRemoteAddress, getRemotePort, getTwiddleTimeout, isNewConnection, setDefaultTimeout, setIsSecure, setLocalHostname, setLogAccess, setLogError, setNumAcceptLoops, setOnDataFinished, setOnEscape, setOnException, setOnNewRequest, setOnParse, setOnUserHandlerFinished)
import           Snap.Internal.Http.Server.Date    (getLogDateString)
import           Snap.Internal.Http.Server.Session (BadRequestException (..), LengthRequiredException (..), TerminateSessionException (..), httpAcceptLoop, httpSession, snapToServerHandler, sendResponse)
import qualified Snap.Internal.Http.Server.TLS     as TLS
import           Snap.Internal.Http.Server.Types   (AcceptFunc (AcceptFunc), PerSessionData (PerSessionData, _isNewConnection), SendFileHandler, ServerConfig (_logError))
import           Snap.Internal.Http.Types          (Request(..), Method(..), Response(..))
import qualified Snap.Internal.Http.Types          as SnapHttpTypes
import           Snap.Test                         (RequestBuilder)
import qualified Snap.Test                         as T
import           Snap.Test.Common                  (coverShowInstance, coverTypeableInstance, expectException)
import qualified Snap.Types.Headers         as H
#ifdef OPENSSL
import qualified Network.Socket                    as N
#endif


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testPong
        , testPong1_0
        , testDateHeaderDeleted
        , testServerHeader
        , testBadParses
        , testEof
        , testHttp100
        , testNoHost
        , testNoHost1_0
        , testChunkedRequest
        , testQueryParams
        , testPostParams
        , testPostParamsReplacementBody
        , testCookie
        , testSetCookie
        , testUserException
        , testUserBodyException
        , testEscape
        , testPostWithoutLength
        , testWeirdMissingSlash
        , testOnlyQueryString
        , testConnectionClose
        , testUserTerminate
        , testSendFile
        , testBasicAcceptLoop
        , testTrivials
        , testBodySize
#ifdef OPENSSL
        , testTLSKeyMismatch
#else
        , testCoverTLSStubs
#endif
        ]


------------------------------------------------------------------------------
#ifdef OPENSSL
testTLSKeyMismatch :: Test
testTLSKeyMismatch = testCase "session/tls-key-mismatch" $ do
    expectException $ bracket (TLS.bindHttps "127.0.0.1"
                                    (fromIntegral N.aNY_PORT)
                                    "test/cert.pem"
                                    False
                                    "test/bad_key.pem")
                              (N.close . fst)
                              (const $ return ())
    expectException $ bracket (TLS.bindHttps "127.0.0.1"
                                    (fromIntegral N.aNY_PORT)
                                    "test/cert.pem"
                                    True
                                    "test/bad_key.pem")
                              (N.close . fst)
                              (const $ return ())
#else
testCoverTLSStubs :: Test
testCoverTLSStubs = testCase "session/tls-stubs" $ do
    expectException $ TLS.bindHttps "127.0.0.1" 9999
                        "test/cert.pem" False "test/key.pem"
    expectException $ TLS.bindHttps "127.0.0.1" 9999
                        "test/cert.pem" True "test/key.pem"
    let (AcceptFunc afunc) = TLS.httpsAcceptFunc undefined undefined
    expectException $ mask $ \restore -> afunc restore
    let u = undefined
    expectException $ TLS.sendFileFunc u u u u u u u
#endif

mkDefaultRequest :: IO Request
mkDefaultRequest = do
    b <- Streams.fromList $! []
    return $ Request "localhost"
                     "127.0.0.1"
                     60000
                     "127.0.0.1"
                     8080
                     "localhost"
                     False
                     H.empty
                     b
                     Nothing
                     GET
                     (1,1)
                     []
                     ""
                     "/"
                     "/"
                     ""
                     Map.empty
                     Map.empty
                     Map.empty

testBodySize :: Test
testBodySize = testCase "session/bodySize" $ do
    req <- mkDefaultRequest
    outputStream <- makeOutputStream (const (return ()))
    let tickle = const (return ())
    let sendfileHandler _ _ _ _ _ = return ()
    forceConnectionClose <- newIORef True
    let rspbody = SnapHttpTypes.Stream return
    let rsp = Response H.empty Map.empty Nothing rspbody 200 "" False
    buf <- newBuffer 10
    size <- sendResponse outputStream 100 tickle buf sendfileHandler forceConnectionClose req rsp
    assertEqual "responseSize" 123 size

------------------------------------------------------------------------------
testPong :: Test
testPong = testCase "session/pong" $ do
    do
      [(resp, body)] <- runRequestPipeline [return ()] snap1
      assertEqual "code1" 200 $ Http.getStatusCode resp
      assertEqual "body1" pong body
      assertEqual "chunked1" Nothing $
                            Http.getHeader resp "Transfer-Encoding"
    do
      [(resp, body)] <- runRequestPipeline [return ()] snap2
      assertEqual "code2" 200 $ Http.getStatusCode resp
      assertEqual "body2" pong body
      assertEqual "chunked2" (Just $ CI.mk "chunked") $
                             fmap CI.mk $
                             Http.getHeader resp "Transfer-Encoding"
    -- test pipelining
    do
      [_, (resp, body)] <- runRequestPipeline [return (), return ()] snap3
      assertEqual "code3" 233 $ Http.getStatusCode resp
      assertEqual "reason3" "ZOMG" $ Http.getStatusMessage resp
      assertEqual "body3" pong body
      assertEqual "chunked3" Nothing $ Http.getHeader resp "Transfer-Encoding"

    do
      [_, (resp, body)] <- runRequestPipeline [http_1_0, http_1_0] snap3
      assertEqual "code4" 233 $ Http.getStatusCode resp
      assertEqual "reason4" "ZOMG" $ Http.getStatusMessage resp
      assertEqual "body4" pong body
      assertEqual "chunked4" Nothing $ Http.getHeader resp "Transfer-Encoding"

  where
    http_1_0 = do
        T.setHttpVersion (1, 0)
        T.setHeader "Connection" "keep-alive"
    pong = "PONG"

    snap1 = writeBS pong >> modifyResponse (setContentLength 4)
    snap2 = do
        cookies <- getsRequest rqCookies
        if null cookies
          then writeBS pong
          else writeBS "wat"

    snap3 = do
        modifyResponse (setResponseStatus 233 "ZOMG" . setContentLength 4)
        writeBS pong

------------------------------------------------------------------------------
testPong1_0 :: Test
testPong1_0 = testCase "session/pong1_0" $ do
    req <- makeRequest (T.setHttpVersion (1, 0) >>
                        T.setHeader "Connection" "zzz")
    out <- getSessionOutput [req] $ writeBS "PONG"
    assertBool "200 ok" $ S.isPrefixOf "HTTP/1.0 200 OK\r\n" out
    assertBool "PONG" $ S.isSuffixOf "\r\n\r\nPONG" out


------------------------------------------------------------------------------
testDateHeaderDeleted :: Test
testDateHeaderDeleted = testCase "session/dateHeaderDeleted" $ do
    [(resp, _)] <- runRequestPipeline [mkRq] snap
    assertBool "date header" $ Just "plop" /= Http.getHeader resp "Date"

    [_, (resp2, _)] <- runRequestPipeline [mkRq2, mkRq2] snap
    assertBool "date header 2" $ Just "plop" /= Http.getHeader resp2 "Date"

  where
    snap = do
        modifyResponse (setHeader "Date" "plop" .
                        setHeader "Connection" "ok" .
                        setContentLength 4)
        writeBS "PONG"

    mkRq = do
        T.setHttpVersion (1,0)
        T.setHeader "fnargle" "plop"
        T.setHeader "Content-Length" "0"
        T.setHeader "Connection" "keep-alive"

    mkRq2 = do
        T.setHeader "fnargle" "plop"
        T.setHeader "Content-Length" "0"
        T.setHeader "Connection" "keep-alive"


------------------------------------------------------------------------------
testServerHeader :: Test
testServerHeader = testCase "session/serverHeader" $ do
    [(resp, _)] <- runRequestPipeline [return ()] snap
    assertEqual "server" (Just "blah") $ Http.getHeader resp "Server"
  where
    snap = modifyResponse $ setHeader "Server" "blah"


------------------------------------------------------------------------------
testBadParses :: Test
testBadParses = testGroup "session/badParses" [
                  check 1 "Not an HTTP Request"
                , check 2 $ S.concat [ "GET / HTTP/1.1\r\n"
                                     , "&*%^(*&*@YS\r\n\r324932\n)"
                                     ]
                , check 3 "\n"
                ]

  where
    check :: Int -> ByteString -> Test
    check n txt = testCase ("session/badParses/" ++ show n) $
                  expectException $ getSessionOutput [txt] (return ())


------------------------------------------------------------------------------
testEof :: Test
testEof = testCase "session/eof" $ do
    l <- runRequestPipeline [] snap
    assertBool "eof1" $ null l

    out <- getSessionOutput [""] snap
    assertEqual "eof2" "" out
  where
    snap = writeBS "OK"


------------------------------------------------------------------------------
testHttp100 :: Test
testHttp100 = testCase "session/expect100" $ do
    req <- makeRequest expect100
    out <- getSessionOutput [req] (writeBS "OK")

    assertBool "100-continue" $
               S.isPrefixOf "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK" out

    req2 <- makeRequest expect100_2
    out2 <- getSessionOutput [req2] (writeBS "OK")

    assertBool "100-continue-2" $
               S.isPrefixOf "HTTP/1.0 100 Continue\r\n\r\nHTTP/1.0 200 OK" out2

  where
    expect100 = do
        queryGetParams
        T.setHeader "Expect" "100-continue"

    expect100_2 = do
        T.setHttpVersion (1, 0)
        queryGetParams
        T.setHeader "Expect" "100-continue"


------------------------------------------------------------------------------
testNoHost :: Test
testNoHost = testCase "session/noHost" $
             expectException $
             getSessionOutput ["GET / HTTP/1.1\r\n\r\n"] (writeBS "OK")


------------------------------------------------------------------------------
testNoHost1_0 :: Test
testNoHost1_0 = testCase "session/noHost1_0" $ do
    out <- getSessionOutput ["GET / HTTP/1.0\r\n\r\n"] snap1
    assertBool "no host 1.0" $ S.isSuffixOf "\r\nbackup-localhost" out
    out2 <- getSessionOutput ["GET / HTTP/1.0\r\n\r\n"] snap2
    assertBool "no host 1.0-2" $ S.isSuffixOf "\r\nbackup-localhost" out2
  where
    snap1 = getRequest >>= writeBS . rqHostName
    snap2 = getRequest >>= writeBS . rqLocalHostname


------------------------------------------------------------------------------
testChunkedRequest :: Test
testChunkedRequest = testCase "session/chunkedRequest" $ do
    [(_, body)] <- runRequestPipeline [chunked] snap
    assertEqual "chunked" "ok" body
  where
    snap = do
        m <- liftM (getHeader "Transfer-Encoding") getRequest
        if m == Just "chunked"
          then readRequestBody 2048 >>= writeLBS
          else writeBS "not ok"

    chunked = do
        T.put "/" "text/plain" "ok"
        T.setHeader "Transfer-Encoding" "chunked"


------------------------------------------------------------------------------
testQueryParams :: Test
testQueryParams = testCase "session/queryParams" $ do
    [(_, body)] <- runRequestPipeline [queryGetParams] snap
    assertEqual "queryParams" expected body

  where
    expected = S.unlines [
                 "param1=abc,def"
               , "param2=def"
               , "param1=abc,def"
               , "ok"
               ]
    snap = do
        rq <- getRequest
        let (Just l) = rqParam "param1" rq
        writeBS $ S.concat [ "param1="
                           , S.intercalate "," l
                           , "\n" ]
        let (Just m) = rqParam "param2" rq
        writeBS $ S.concat [ "param2="
                           , S.intercalate "," m
                           , "\n"]
        let (Just l') = rqQueryParam "param1" rq
        writeBS $ S.concat [ "param1="
                           , S.intercalate "," l'
                           , "\n" ]
        let z = if isNothing $ rqPostParam "param1" rq
                  then "ok\n" else "bad\n"
        writeBS z

        return ()


------------------------------------------------------------------------------
testPostParams :: Test
testPostParams = testCase "session/postParams" $ do
    [(_, body)] <- runRequestPipeline [queryPostParams] snap
    assertEqual "postParams" expected body

  where
    expected = S.unlines [
                 "param1=abc,abc"
               , "param2=def  ,zzz"
               , "param1=abc,abc"
               , "ok"
               , "param2=zzz"
               ]
    snap = do
        rq <- getRequest
        let (Just l) = rqParam "param1" rq
        writeBS $ S.concat [ "param1="
                           , S.intercalate "," l
                           , "\n" ]
        let (Just m) = rqParam "param2" rq
        writeBS $ S.concat [ "param2="
                           , S.intercalate "," m
                           , "\n"]
        let (Just l') = rqQueryParam "param1" rq
        writeBS $ S.concat [ "param1="
                           , S.intercalate "," l'
                           , "\n" ]
        let z = if isNothing $ rqPostParam "param1" rq
                  then "ok\n" else "bad\n"
        writeBS z

        let (Just p) = rqPostParam "param2" rq
        writeBS $ S.concat [ "param2="
                           , S.intercalate "," p
                           , "\n" ]

        return ()


------------------------------------------------------------------------------
testPostParamsReplacementBody :: Test
testPostParamsReplacementBody =
  testCase "session/postParamsReplacementBody" $ do
    [(_, body)] <- runRequestPipeline [queryPostParams] snap
    assertEqual "postParams" expected body

  where
    expected = "param2=zzz"
    snap = readRequestBody 2048 >>= writeLBS


------------------------------------------------------------------------------
testCookie :: Test
testCookie = testCase "session/cookie" $ do
    [(_, body)] <- runRequestPipeline [queryGetParams] snap
    assertEqual "cookie" expected body
  where
    expected = S.unlines [ "foo"
                         , "bar"
                         ]
    snap = do
        cookies <- liftM rqCookies getRequest
        forM_ cookies $ \cookie -> do
            writeBS $ S.unlines [ cookieName cookie
                                , cookieValue cookie
                                ]


------------------------------------------------------------------------------
testSetCookie :: Test
testSetCookie = testCase "session/setCookie" $ do
    mapM_ runTest $ zip3 [1..] expecteds cookies
  where
    runTest (n, expected, cookie) = do
        [(resp, _)] <- runRequestPipeline [queryGetParams] $ snap cookie
        assertEqual ("cookie" ++ show (n :: Int))
                    (Just expected)
                    (Http.getHeader resp "Set-Cookie")

    expecteds = [ S.intercalate "; "
                    [ "foo=bar"
                    , "path=/"
                    , "expires=Thu, 01-Jan-1970 00:16:40 GMT"
                    , "domain=localhost"
                    ]
                , "foo=bar"
                , "foo=bar; Secure; HttpOnly"
                ]

    cookies = [ Cookie "foo" "bar" (Just $ posixSecondsToUTCTime 1000)
                       (Just "localhost") (Just "/") False False
              , Cookie "foo" "bar" Nothing Nothing Nothing False False
              , Cookie "foo" "bar" Nothing Nothing Nothing True True
              ]

    snap cookie = do
        modifyResponse $ addResponseCookie cookie


------------------------------------------------------------------------------
testUserException :: Test
testUserException = testCase "session/userException" $ do
    expectException $ runRequestPipeline [queryGetParams] snap
  where
    snap = throwIO TestException


------------------------------------------------------------------------------
testUserBodyException :: Test
testUserBodyException = testCase "session/userBodyException" $ do
    expectException $ runRequestPipeline [queryGetParams] snap
  where
    snap = modifyResponse $ setResponseBody $ \os -> do
        Streams.write (Just (byteString "hi" `mappend` flush)) os
        throwIO TestException


------------------------------------------------------------------------------
testEscape :: Test
testEscape = testCase "session/testEscape" $ do
    req <- makeRequest (return ())
    out <- getSessionOutput [req, "OK?"] snap
    assertEqual "escape" "OK" out
  where
    snap = escapeHttp $ \tickle readEnd writeEnd -> do
         l <- Streams.toList readEnd
         tickle (max 20)
         let s = if l == ["OK?"]
                   then "OK"
                   else S.append "BAD: " $ S.pack $ show l
         Streams.write (Just $ byteString s) writeEnd
         Streams.write Nothing writeEnd


------------------------------------------------------------------------------
testPostWithoutLength :: Test
testPostWithoutLength = testCase "session/postWithoutLength" $ do
    let req = S.concat [ "POST / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                       , "Blah blah blah blah blah"
                       ]

    is <- Streams.fromList [req]
    (os, getInput) <- listOutputStream
    expectException $ runSession is os (return ())
    out <- liftM S.concat getInput
    assertBool "post without length" $
        S.isPrefixOf "HTTP/1.1 411 Length Required" out


------------------------------------------------------------------------------
testWeirdMissingSlash :: Test
testWeirdMissingSlash = testCase "session/weirdMissingSlash" $ do
  do
    let req = "GET foo/bar?z HTTP/1.0\r\n\r\n"
    out <- getSessionOutput [req] snap
    assertBool "missing slash" $ expected1 `S.isSuffixOf` out
  do
    let req = "GET /foo/bar?z HTTP/1.0\r\n\r\n"
    out <- getSessionOutput [req] snap
    assertBool "with slash" $ expected2 `S.isSuffixOf` out

  where
    expected1 = S.concat [ "\r\n\r\n"
                         , "foo/bar?z\n"
                         , "foo/bar\n"
                         , "z\n"
                         ]
    expected2 = S.concat [ "\r\n\r\n"
                         , "/foo/bar?z\n"
                         , "foo/bar\n"
                         , "z\n"
                         ]
    p s = writeBuilder $ byteString s `mappend` char8 '\n'
    snap = do
        rq <- getRequest
        p $ rqURI rq
        p $ rqPathInfo rq
        p $ rqQueryString rq


------------------------------------------------------------------------------
testOnlyQueryString :: Test
testOnlyQueryString = testCase "session/onlyQueryString" $ do
  do
    let req = "GET ?z HTTP/1.0\r\n\r\n"
    out <- getSessionOutput [req] snap
    assertBool "missing slash" $ expected `S.isSuffixOf` out
  where
    expected = S.concat [ "\r\n\r\n"
                         , "?z\n"
                         , "\n"
                         , "z\n"
                         ]
    p s = writeBuilder $ byteString s `mappend` char8 '\n'
    snap = do
        rq <- getRequest
        p $ rqURI rq
        p $ rqPathInfo rq
        p $ rqQueryString rq


------------------------------------------------------------------------------
testConnectionClose :: Test
testConnectionClose = testCase "session/connectionClose" $ do
    do
      [(resp, _)] <- runRequestPipeline [return (), return ()] snap
      assertEqual "close1" (Just $ CI.mk "close") $
                           fmap CI.mk $
                           Http.getHeader resp "Connection"
    do
      [(resp, _)] <- runRequestPipeline [http1_0, http1_0] snap
      assertEqual "close2" (Just $ CI.mk "close") $
                           fmap CI.mk $
                           Http.getHeader resp "Connection"

    do
      [(resp, _)] <- runRequestPipeline [http1_0_2, http1_0] (return ())
      assertEqual "close3" (Just $ CI.mk "close") $
                           fmap CI.mk $
                           Http.getHeader resp "Connection"

  where
    http1_0 = T.setHttpVersion (1, 0)
    http1_0_2 = T.setHttpVersion (1, 0) >> T.setHeader "Connection" "fnargle"
    snap  = modifyResponse $ setHeader "Connection" "close"


------------------------------------------------------------------------------
testUserTerminate :: Test
testUserTerminate = testCase "session/userTerminate" $ do
    expectException $ runRequestPipeline [return ()] snap
  where
    snap = terminateConnection TestException


------------------------------------------------------------------------------
testSendFile :: Test
testSendFile = testCase "session/sendFile" $ do
    [(_, out1)] <- runRequestPipeline [return ()] snap1
    [(_, out2)] <- runRequestPipeline [return ()] snap2
    assertEqual "sendfile1" "TESTING 1-2-3\n" out1
    assertEqual "sendfile2" "EST" out2
  where
    snap1 = sendFile "test/dummy.txt"
    snap2 = sendFilePartial "test/dummy.txt" (1,4)


------------------------------------------------------------------------------
testBasicAcceptLoop :: Test
testBasicAcceptLoop = testCase "session/basicAcceptLoop" $
                      replicateM_ 1000 $ do
    outputs <- runAcceptLoop [return ()] (return ())
    let [Output out] = outputs
    void (evaluate out) `catch` \(e::SomeException) -> do
        throwIO e
    assertBool "basic accept" $ S.isPrefixOf "HTTP/1.1 200 OK\r\n" out


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "session/trivials" $ do
    coverShowInstance $ TerminateSessionException
                      $ SomeException BadRequestException
    coverShowInstance LengthRequiredException
    coverShowInstance BadRequestException
    coverShowInstance $ TLS.TLSException "ok"
    coverTypeableInstance (undefined :: TerminateSessionException)
    coverTypeableInstance (undefined :: BadRequestException)
    coverTypeableInstance (undefined :: LengthRequiredException)
    coverTypeableInstance (undefined :: TLS.TLSException)

    expectException (getOnNewRequest emptyServerConfig undefined >>= evaluate)
    is <- Streams.fromList []
    (os, _) <- Streams.listOutputStream
    psd <- makePerSessionData is os
    isNewConnection psd >>= assertEqual "new connection" False

    -- cover getters
    let !_ = getTwiddleTimeout psd
    let !_ = getRemotePort psd
    let !_ = getRemoteAddress psd
    let !_ = getLocalPort psd
    let !_ = getLocalAddress psd
    getOnParse emptyServerConfig undefined undefined
    getOnEscape emptyServerConfig undefined
    getOnException emptyServerConfig undefined undefined
    getOnDataFinished emptyServerConfig undefined undefined undefined
    getOnUserHandlerFinished emptyServerConfig undefined undefined undefined
    getLogError emptyServerConfig undefined
    getLogAccess emptyServerConfig undefined undefined undefined
    let !_ = getLogError emptyServerConfig
    let !_ = getLocalHostname emptyServerConfig
    let !_ = getDefaultTimeout emptyServerConfig
    let !_ = getNumAcceptLoops emptyServerConfig
    let !_ = getIsSecure emptyServerConfig

    !x <- getLogDateString
    threadDelay $ 2 * seconds
    !y <- getLogDateString
    assertBool (concat ["log dates: ", show x, ", ", show y]) $ x /= y


                             ---------------------
                             -- query fragments --
                             ---------------------

------------------------------------------------------------------------------
queryGetParams :: RequestBuilder IO ()
queryGetParams = do
    T.get "/foo/bar.html" $ Map.fromList [ ("param1", ["abc", "def"])
                                         , ("param2", ["def"])
                                         ]
    T.addCookies [ Cookie "foo" "bar" Nothing (Just "localhost") (Just "/")
                          False False ]
    modify $ \rq -> rq { rqContentLength = Just 0 }


------------------------------------------------------------------------------
queryPostParams :: RequestBuilder IO ()
queryPostParams = do
    T.postUrlEncoded "/" $ Map.fromList [ ("param2", ["zzz"]) ]
    T.setQueryStringRaw "param1=abc&param2=def%20+&param1=abc"


                            -----------------------
                            -- utility functions --
                            -----------------------

------------------------------------------------------------------------------
_run :: [Test] -> IO ()
_run l = Console.defaultMainWithArgs l ["--plain"]


------------------------------------------------------------------------------
-- | Given a request builder, produce the HTTP request as a ByteString.
makeRequest :: RequestBuilder IO a -> IO ByteString
makeRequest = (T.buildRequest . void) >=> T.requestToString


------------------------------------------------------------------------------
mockSendFileHandler :: OutputStream ByteString -> SendFileHandler
mockSendFileHandler os !_ hdrs fp start nbytes = do
    let hstr = toByteString hdrs
    Streams.write (Just hstr) os

    Streams.withFileAsInputStartingAt (fromIntegral start) fp $
        Streams.takeBytes (fromIntegral nbytes) >=> Streams.supplyTo os

    Streams.write Nothing os


------------------------------------------------------------------------------
-- | Fill in a 'PerSessionData' with some dummy values.
makePerSessionData :: InputStream ByteString
                   -> OutputStream ByteString
                   -> IO PerSessionData
makePerSessionData readEnd writeEnd = do
    forceConnectionClose <- newIORef False
    let twiddleTimeout f = let z = f 0 in z `seq` return $! ()
    let localAddress = "127.0.0.1"
    let remoteAddress = "127.0.0.1"
    let remotePort = 43321
    newConnectionRef <- newIORef False

    let psd = PerSessionData forceConnectionClose
                             twiddleTimeout
                             newConnectionRef
                             (mockSendFileHandler writeEnd)
                             localAddress
                             80
                             remoteAddress
                             remotePort
                             readEnd
                             writeEnd

    return psd


------------------------------------------------------------------------------
-- | Make a pipe -- the two Input/OutputStream pairs will communicate with each
-- other from separate threads by using 'Chan's.
makePipe :: PipeFunc
makePipe = do
    chan1 <- newChan
    chan2 <- newChan

    clientReadEnd  <- Streams.chanToInput  chan1
    clientWriteEnd <- Streams.chanToOutput chan2 >>=
                      Streams.contramapM (evaluate . S.copy)
    serverReadEnd  <- Streams.chanToInput  chan2
    serverWriteEnd <- Streams.chanToOutput chan1 >>=
                      Streams.contramapM (evaluate . S.copy)

    return ((clientReadEnd, clientWriteEnd), (serverReadEnd, serverWriteEnd))


------------------------------------------------------------------------------
-- | Make a pipe -- the two Input/OutputStream pairs will communicate with each
-- other from separate threads by using 'Chan's. Data moving through the
-- streams will be logged to stdout.
_makeDebugPipe :: ByteString -> PipeFunc
_makeDebugPipe name = do
    chan1 <- newChan
    chan2 <- newChan

    clientReadEnd  <- Streams.chanToInput  chan1 >>=
                      Streams.debugInputBS (S.append name "/client-rd")
                                           Streams.stderr
    clientWriteEnd <- Streams.chanToOutput chan2 >>=
                      Streams.debugOutputBS (S.append name "/client-wr")
                                            Streams.stderr >>=
                      Streams.contramapM (evaluate . S.copy)
    serverReadEnd  <- Streams.chanToInput  chan2 >>=
                      Streams.debugInputBS (S.append name "/server-rd")
                                           Streams.stderr
    serverWriteEnd <- Streams.chanToOutput chan1 >>=
                      Streams.debugOutputBS (S.append name "/server-wr")
                                            Streams.stderr >>=
                      Streams.contramapM (evaluate . S.copy)

    return ((clientReadEnd, clientWriteEnd), (serverReadEnd, serverWriteEnd))


------------------------------------------------------------------------------
type PipeFunc = IO ( (InputStream ByteString, OutputStream ByteString)
                   , (InputStream ByteString, OutputStream ByteString)
                   )

------------------------------------------------------------------------------
-- | Given a bunch of requests, convert them to bytestrings and pipeline them
-- into the 'httpSession' code, recording the results.
runRequestPipeline :: [T.RequestBuilder IO ()]
                   -> Snap b
                   -> IO [(Http.Response, ByteString)]
runRequestPipeline = runRequestPipelineDebug makePipe


------------------------------------------------------------------------------
-- | Given a bunch of requests, convert them to bytestrings and pipeline them
-- into the 'httpSession' code, recording the results.
runRequestPipelineDebug :: PipeFunc
                        -> [T.RequestBuilder IO ()]
                        -> Snap b
                        -> IO [(Http.Response, ByteString)]
runRequestPipelineDebug pipeFunc rbs handler = dieIfTimeout $ do
    ((clientRead, clientWrite), (serverRead, serverWrite)) <- pipeFunc

    sigClient <- newEmptyMVar
    results   <- newMVar []

    forM_ rbs $ makeRequest >=> flip Streams.write clientWrite . Just
    Streams.write Nothing clientWrite

    myTid <- myThreadId
    conn <- Http.makeConnection "localhost"
                                (return ())
                                clientWrite
                                clientRead

    bracket (do ctid <- mask $ \restore ->
                        forkIO $ clientThread restore myTid clientRead conn
                                              results sigClient
                stid <- forkIO $ serverThread myTid serverRead serverWrite
                return (ctid, stid))
            (\(ctid, stid) -> mapM_ killThread [ctid, stid])
            (\_ -> await sigClient)
    readMVar results

  where
    await sig = takeMVar sig >>= either throwIO (const $ return ())

    serverThread myTid serverRead serverWrite = do
        runSession serverRead serverWrite handler
          `catch` \(e :: SomeException) -> throwTo myTid e

    clientThread restore myTid clientRead conn results sig =
        (try (restore loop) >>= putMVar (sig :: MVar (Either SomeException ())))
          `catch` \(e :: SomeException) -> throwTo myTid e

      where
        loop = do
            eof <- Streams.atEOF clientRead
            if eof
              then return ()
              else do
                (resp, body) <- Http.receiveResponse conn $ \rsp istr -> do
                    !out <- liftM S.concat $ Streams.toList istr
                    return (rsp, out)
                modifyMVar_ results (return . (++ [(resp, body)]))
                loop


------------------------------------------------------------------------------
getSessionOutput :: [ByteString]
                 -> Snap a
                 -> IO ByteString
getSessionOutput input snap = do
    is <- Streams.fromList input >>= Streams.mapM (evaluate . S.copy)
    (os0, getList) <- Streams.listOutputStream
    os <- Streams.contramapM (evaluate . S.copy) os0
    runSession is os snap
    liftM S.concat getList


------------------------------------------------------------------------------
runSession :: InputStream ByteString
           -> OutputStream ByteString
           -> Snap a
           -> IO ()
runSession readEnd writeEnd handler = do
    buffer         <- newBuffer 64000
    perSessionData <- makePerSessionData readEnd writeEnd
    httpSession buffer (snapToServerHandler handler)
                       (makeServerConfig ())
                       perSessionData
    Streams.write Nothing writeEnd


------------------------------------------------------------------------------
makeServerConfig :: hookState -> ServerConfig hookState
makeServerConfig hs = setOnException onEx                            .
                      setOnNewRequest onStart                        .
                      setLogError logErr                             .
                      setLogAccess logAccess                         .
                      setOnDataFinished onDataFinished               .
                      setOnEscape onEscape                           .
                      setOnUserHandlerFinished onUserHandlerFinished .
                      setDefaultTimeout 10                           .
                      setLocalHostname "backup-localhost"            .
                      setIsSecure False                              .
                      setNumAcceptLoops 1                            .
                      setOnParse onParse                             $
                      emptyServerConfig
  where
    onStart !psd = do
        void $ readIORef (_isNewConnection psd) >>= evaluate
        return hs

    logAccess !_ !_ !_             = return $! ()
    logErr !e                      = void $ evaluate $ toByteString e
    onParse !_ !_                  = return $! ()
    onUserHandlerFinished !_ !_ !_ = return $! ()
    onDataFinished !_ !_ !_        = return $! ()
    onEx !_ !e                     = throwIO e
    onEscape !_                    = return $! ()


------------------------------------------------------------------------------
listOutputStream :: IO (OutputStream ByteString, IO [ByteString])
listOutputStream = do
    (os, out) <- Streams.listOutputStream
    os' <- Streams.contramapM (evaluate . S.copy) os
    return (os', out)


------------------------------------------------------------------------------
data TestException = TestException
  deriving (Typeable, Show)
instance Exception TestException


------------------------------------------------------------------------------
data Result = SendFile ByteString FilePath Word64 Word64
            | Output ByteString
  deriving (Eq, Ord, Show)


------------------------------------------------------------------------------
runAcceptLoop :: [T.RequestBuilder IO ()]
              -> Snap a
              -> IO [Result]
runAcceptLoop requests snap = dieIfTimeout $ do
    -- make sure we don't log error on ThreadKilled.
    (_, errs') <- run afuncSuicide
    assertBool ("errs': " ++ show errs') $ null errs'

    -- make sure we gobble IOException.
    count <- newIORef 0
    (_, errs'') <- run $ afuncIOException count
    assertBool ("errs'': " ++ show errs'') $ length errs'' == 2

    liftM fst $ run acceptFunc

  where
    --------------------------------------------------------------------------
    run afunc = do
        reqStreams <- Streams.fromList requests >>=
                      Streams.mapM makeRequest >>=
                      Streams.lockingInputStream

        outputs  <- newMVar []
        lock     <- newMVar ()
        err      <- newMVar []

        httpAcceptLoop (snapToServerHandler snap) (config err) $
                       afunc reqStreams outputs lock

        out <- takeMVar outputs
        errs <- takeMVar err
        return (out, errs)

    --------------------------------------------------------------------------
    config mvar = (makeServerConfig ()) {
                    _logError = \b -> let !s = toByteString b
                                      in modifyMVar_ mvar $ \xs -> do
                                           void (evaluate s)
                                           return (xs ++ [s])
                  }

    --------------------------------------------------------------------------
    afuncSuicide :: InputStream ByteString
                 -> MVar [Result]
                 -> MVar ()
                 -> AcceptFunc
    afuncSuicide _ _ lock = AcceptFunc $ \restore ->
        restore $ withMVar lock (\_ -> throwIO ThreadKilled)

    --------------------------------------------------------------------------
    afuncIOException :: IORef Int
                     -> InputStream ByteString
                     -> MVar [Result]
                     -> MVar ()
                     -> AcceptFunc
    afuncIOException ref _ _ lock = AcceptFunc $ \restore ->
                                    restore $ withMVar lock $ const $ do
        x <- readIORef ref
        writeIORef ref $! x + 1
        if x >= 2
          then throwIO ThreadKilled
          else throwIO $ userError "hello"

    --------------------------------------------------------------------------
    acceptFunc :: InputStream ByteString
               -> MVar [Result]
               -> MVar ()
               -> AcceptFunc
    acceptFunc inputStream output lock = AcceptFunc $ \restore -> restore $ do
        void $ takeMVar lock
        b <- atEOF
        when b $ myThreadId >>= killThread
        os <- Streams.makeOutputStream out >>=
              Streams.contramap S.copy
        return (sendFileFunc, "localhost", 80, "localhost", 55555, inputStream,
                os, putMVar lock ())

      where
        atEOF = Streams.peek inputStream >>= maybe (return True) f
          where
            f x | S.null x  = do void $ Streams.read inputStream
                                 atEOF
                | otherwise = return False

        out (Just s) | S.null s = return ()
        out (Just s)            = modifyMVar_ output $ return . (++ [Output s])
        out Nothing             = return ()

        sendFileFunc !_ !bldr !fp !st !end =
          modifyMVar_ output $
          return . (++ [(SendFile (toByteString bldr) fp st end)])


------------------------------------------------------------------------------
dieIfTimeout :: IO a -> IO a
dieIfTimeout m = timeout (10 * seconds) m >>= maybe (error "timeout") return


------------------------------------------------------------------------------
seconds :: Int
seconds = (10::Int) ^ (6::Int)

------------------------------------------------------------------------------
toByteString :: Builder -> S.ByteString
toByteString = S.concat . L.toChunks . toLazyByteString
