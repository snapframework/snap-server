{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Snap.Internal.Http.Server.Tests
  ( tests ) where

import             Control.Concurrent
import             Control.Exception ( try
                                     , throwIO
                                     , bracket
                                     , finally
                                     , SomeException
                                     , Exception )
import             Control.Monad
import "monads-fd" Control.Monad.Trans
import qualified   Data.ByteString.Char8 as S
import qualified   Data.ByteString.Lazy as L
import qualified   Data.ByteString.Lazy.Char8 as LC
import             Data.ByteString (ByteString)
import             Data.ByteString.Internal (c2w)
import             Data.Char
import             Data.Int
import             Data.IORef
import qualified   Data.Map as Map
import             Data.Maybe (fromJust)
import             Data.Time.Calendar
import             Data.Time.Clock
import             Data.Typeable
import             Data.Word
import qualified   Network.HTTP as HTTP
import qualified   Network.Socket.ByteString as N
import             Prelude hiding (take)
import qualified   Prelude
import             System.Timeout
import             Test.Framework
import             Test.Framework.Providers.HUnit
import             Test.HUnit hiding (Test, path)

import qualified   Snap.Http.Server as Svr

import             Snap.Internal.Debug
import             Snap.Internal.Http.Types
import             Snap.Internal.Http.Server
import qualified   Snap.Iteratee as I
import             Snap.Iteratee hiding (map)
import             Snap.Internal.Http.Server.Backend
import             Snap.Test.Common
import             Snap.Types

data TestException = TestException
  deriving (Show, Typeable)
instance Exception TestException


tests :: [Test]
tests = [ testHttpRequest1
        , testMultiRequest
        , testHttpRequest2
        , testHttpRequest3
        , testHttpRequest3'
        , testHttpResponse1
        , testHttpResponse2
        , testHttpResponse3
        , testHttpResponse4
        , testHttp1
        , testHttp2
        , testHttp100
        , testExpectGarbage
        , testPartialParse
        , testMethodParsing
        , testServerStartupShutdown
        , testServerShutdownWithOpenConns
        , testChunkOn1_0
        , testSendFile
        , testTrivials]


testTrivials :: Test
testTrivials = testCase "server/trivials" $ do
    let !v = Svr.snapServerVersion
    return $! v `seq` ()

------------------------------------------------------------------------------
-- HTTP request tests

-- note leading crlf -- test tolerance of this, some old browsers send an extra
-- crlf after a post body
sampleRequest :: ByteString
sampleRequest =
    S.concat [ "\r\nGET /foo/bar.html?param1=abc&param2=def%20+&param1=abc HTTP/1.1\r\n"
             , "Host: www.zabble.com:7777\r\n"
             , "Content-Length: 10\r\n"
             , "X-Random-Other-Header: foo\r\n bar\r\n"
             , "Cookie: foo=\"bar\\\"\"\r\n"
             , "\r\n"
             , "0123456789" ]

sampleRequestExpectContinue :: ByteString
sampleRequestExpectContinue =
    S.concat [ "\r\nGET /foo/bar.html?param1=abc&param2=def%20+&param1=abc HTTP/1.1\r\n"
             , "Host: www.zabble.com:7777\r\n"
             , "Content-Length: 10\r\n"
             , "Expect: 100-continue\r\n"
             , "X-Random-Other-Header: foo\r\n bar\r\n"
             , "Cookie: foo=\"bar\\\"\"\r\n"
             , "\r\n"
             , "0123456789" ]

sampleRequestExpectGarbage :: ByteString
sampleRequestExpectGarbage =
    S.concat [ "\r\nGET /foo/bar.html?param1=abc&param2=def%20+&param1=abc HTTP/1.1\r\n"
             , "Host: www.zabble.com:7777\r\n"
             , "Content-Length: 10\r\n"
             , "Expect: wuzzawuzzawuzza\r\n"
             , "X-Random-Other-Header: foo\r\n bar\r\n"
             , "Cookie: foo=\"bar\\\"\"\r\n"
             , "\r\n"
             , "0123456789" ]

sampleRequest1_0 :: ByteString
sampleRequest1_0 =
    S.concat [ "\r\nGET /foo/bar.html?param1=abc&param2=def%20+&param1=abc HTTP/1.0\r\n"
             , "Host: www.zabble.com:7777\r\n"
             , "Content-Length: 10\r\n"
             , "X-Random-Other-Header: foo\r\n bar\r\n"
             , "Cookie: foo=\"bar\\\"\"\r\n"
             , "\r\n"
             , "0123456789" ]

testMethodParsing :: Test
testMethodParsing =
    testCase "server/method parsing" $ Prelude.mapM_ testOneMethod ms
  where
    ms = [ GET, HEAD, POST, PUT, DELETE, TRACE, OPTIONS, CONNECT ]



mkRequest :: ByteString -> IO Request
mkRequest s = do
    step <- runIteratee $ liftM fromJust $ rsm receiveRequest
    let iter = enumBS s step
    run_ iter


testReceiveRequest :: Iteratee ByteString IO (Request,L.ByteString)
testReceiveRequest = do
    r  <- liftM fromJust $ rsm receiveRequest
    se <- liftIO $ readIORef (rqBody r)
    let (SomeEnumerator e) = se
    it  <- liftM e $ lift $ runIteratee copyingStream2Stream
    b   <- it
    return (r,b)


testReceiveRequestIter :: ByteString
                       -> IO (Iteratee ByteString IO (Request,L.ByteString))
testReceiveRequestIter req =
    liftM (enumBS req) $ runIteratee testReceiveRequest


testHttpRequest1 :: Test
testHttpRequest1 =
    testCase "server/HttpRequest1" $ do
        iter <- testReceiveRequestIter sampleRequest

        (req,body) <- run_ iter

        assertEqual "not secure" False $ rqIsSecure req

        assertEqual "content length" (Just 10) $ rqContentLength req

        assertEqual "parse body" "0123456789" body

        assertEqual "cookie" [Cookie "foo" "bar\"" Nothing Nothing Nothing] $
                    rqCookies req

        assertEqual "continued headers" (Just ["foo bar"]) $
                    Map.lookup "x-random-other-header" $ rqHeaders req

        assertEqual "parse URI"
                    "/foo/bar.html?param1=abc&param2=def%20+&param1=abc"
                    $ rqURI req

        assertEqual "server port" 7777 $ rqServerPort req
        assertEqual "context path" "/" $ rqContextPath req
        assertEqual "pathinfo" "foo/bar.html" $ rqPathInfo req
        assertEqual "query string" "param1=abc&param2=def%20+&param1=abc" $
                    rqQueryString req
        assertEqual "server name" "www.zabble.com" $ rqServerName req
        assertEqual "version" (1,1) $ rqVersion req
        assertEqual "param1" (Just ["abc","abc"]) $
                    rqParam "param1" req
        assertEqual "param2" (Just ["def  "]) $
                    rqParam "param2" req



testMultiRequest :: Test
testMultiRequest =
    testCase "server/MultiRequest" $ do
        let clientIter = do
            (r1,b1) <- testReceiveRequest
            (r2,b2) <- testReceiveRequest

            return (r1,b1,r2,b2)

        iter <- liftM (enumBS sampleRequest >==> enumBS sampleRequest) $
                runIteratee clientIter

        (req1,body1,req2,body2) <- run_ iter

        assertEqual "parse body 1" "0123456789" body1
        assertEqual "parse body 2" "0123456789" body2

        assertEqual "parse URI 1"
                    "/foo/bar.html?param1=abc&param2=def%20+&param1=abc"
                    $ rqURI req1

        assertEqual "parse URI 2"
                    "/foo/bar.html?param1=abc&param2=def%20+&param1=abc"
                    $ rqURI req2



testOneMethod :: Method -> IO ()
testOneMethod m = do
    step    <- runIteratee $ liftM fromJust $ rsm receiveRequest
    let iter = enumLBS txt step
    req     <- run_ iter

    assertEqual "method" m $ rqMethod req

  where
    txt = methodTestText m


sampleShortRequest :: ByteString
sampleShortRequest = "GET /fo"

expectException :: IO a -> IO ()
expectException m = do
    e <- try m
    case e of
      Left (_::SomeException)  -> return ()
      Right _ -> assertFailure "expected exception, didn't get it"


testPartialParse :: Test
testPartialParse = testCase "server/short" $ do
    step <- runIteratee $ liftM fromJust $ rsm receiveRequest
    let iter = enumBS sampleShortRequest step

    expectException $ run_ iter


methodTestText :: Method -> L.ByteString
methodTestText m = L.concat [ (L.pack $ map c2w $ show m)
                        , " / HTTP/1.1\r\n\r\n" ]


sampleRequest2 :: ByteString
sampleRequest2 =
    S.concat [ "GET /foo/bar.html?param1=abc&param2=def&param1=abc HTTP/1.1\r\n"
             , "Host: www.foo.com:8080\r\n"
             , "Transfer-Encoding: chunked\r\n"
             , "\r\n"
             , "a\r\n"
             , "0123456789\r\n"
             , "4\r\n"
             , "0123\r\n"
             , "0\r\n\r\n" ]

testHttpRequest2 :: Test
testHttpRequest2 =
    testCase "server/HttpRequest2" $ do
        iter     <- testReceiveRequestIter sampleRequest2
        (_,body) <- run_ iter

        assertEqual "parse body" "01234567890123" body


testHttpRequest3 :: Test
testHttpRequest3 =
    testCase "server/HttpRequest3" $ do
        iter       <- testReceiveRequestIter sampleRequest3
        (req,body) <- run_ iter

        assertEqual "no cookies" [] $ rqCookies req

        assertEqual "multiheader" (Just ["1","2"]) $
                    Map.lookup "Multiheader" (rqHeaders req)

        assertEqual "host" ("localhost", 80) $
                    (rqServerName req, rqServerPort req)

        assertEqual "post param 1"
                    (rqParam "postparam1" req)
                    (Just ["1"])

        assertEqual "post param 2"
                    (rqParam "postparam2" req)
                    (Just ["2"])

        -- make sure the post body is still emitted
        assertEqual "parse body" (LC.fromChunks [samplePostBody3]) body


testHttpRequest3' :: Test
testHttpRequest3' =
    testCase "server/HttpRequest3'" $ do
        iter       <- testReceiveRequestIter sampleRequest3'
        (req,body) <- run_ iter

        assertEqual "post param 1"
                    (rqParam "postparam1" req)
                    (Just ["1"])

        assertEqual "post param 2"
                    (rqParam "postparam2" req)
                    (Just ["2"])

        -- make sure the post body is still emitted
        assertEqual "parse body" (LC.fromChunks [samplePostBody3]) body


samplePostBody3 :: ByteString
samplePostBody3 = "postparam1=1&postparam2=2"


sampleRequest3 :: ByteString
sampleRequest3 =
    S.concat [ "\r\nGET /foo/bar.html?param1=abc&param2=def%20+&param1=abc HTTP/1.1\r\n"
             , "Content-Type: application/x-www-form-urlencoded\r\n"
             , "Content-Length: 25\r\n"
             , "Multiheader: 1\r\n"
             , "Multiheader: 2\r\n"
             , "X-Random-Other-Header: foo\r\n bar\r\n"
             , "\r\n"
             , samplePostBody3 ]


sampleRequest3' :: ByteString
sampleRequest3' =
    S.concat [ "\r\nGET /foo/bar.html?param1=abc&param2=def%20+&param1=abc HTTP/1.1\r\n"
             , "Content-Type: application/x-www-form-urlencoded; charset=UTF-8\r\n"
             , "Content-Length: 25\r\n"
             , "Multiheader: 1\r\n"
             , "Multiheader: 2\r\n"
             , "X-Random-Other-Header: foo\r\n bar\r\n"
             , "\r\n"
             , samplePostBody3 ]




rsm :: ServerMonad a -> Iteratee ByteString IO a
rsm = runServerMonad "localhost" (SessionInfo "127.0.0.1" 80 "127.0.0.1" 58382 False) alog elog
  where
    alog = const . const . return $ ()
    elog = const $ return ()


testHttpResponse1 :: Test
testHttpResponse1 = testCase "server/HttpResponse1" $ do
    sstep <- runIteratee copyingStream2Stream
    req   <- mkRequest sampleRequest

    b     <- run_ $ rsm $
             sendResponse req rsp1 sstep testOnSendFile >>=
                          return . snd

    assertEqual "http response" (L.concat [
                      "HTTP/1.0 600 Test\r\n"
                    , "Content-Length: 10\r\n"
                    , "Foo: Bar\r\n\r\n"
                    , "0123456789"
                    ]) b

  where
    rsp1 = updateHeaders (Map.insert "Foo" ["Bar"]) $
           setContentLength 10 $
           setResponseStatus 600 "Test" $
           modifyResponseBody (>==> (enumBS "0123456789")) $
           setResponseBody returnI $
           emptyResponse { rspHttpVersion = (1,0) }



testOnSendFile :: FilePath -> Int64 -> Int64 -> IO L.ByteString
testOnSendFile f st sz = do
    sstep <- runIteratee copyingStream2Stream
    run_ $ enumFilePartial f (st,st+sz) sstep

testHttpResponse2 :: Test
testHttpResponse2 = testCase "server/HttpResponse2" $ do
    sstep <- runIteratee copyingStream2Stream
    req   <- mkRequest sampleRequest
    b2    <- run_ $ rsm $
             sendResponse req rsp2 sstep testOnSendFile >>=
                          return . snd

    assertEqual "http response" (L.concat [
                      "HTTP/1.0 600 Test\r\n"
                    , "Connection: close\r\n"
                    , "Foo: Bar\r\n\r\n"
                    , "0123456789"
                    ]) b2
  where
    rsp1 = updateHeaders (Map.insert "Foo" ["Bar"]) $
           setContentLength 10 $
           setResponseStatus 600 "Test" $
           modifyResponseBody (>==> (enumBS "0123456789")) $
           setResponseBody returnI $
           emptyResponse { rspHttpVersion = (1,0) }
    rsp2 = rsp1 { rspContentLength = Nothing }


testHttpResponse3 :: Test
testHttpResponse3 = testCase "server/HttpResponse3" $ do
    sstep <- runIteratee copyingStream2Stream
    req   <- mkRequest sampleRequest

    b3 <- run_ $ rsm $
          sendResponse req rsp3 sstep testOnSendFile >>=
                       return . snd

    assertEqual "http response" b3 $ L.concat [
                      "HTTP/1.1 600 Test\r\n"
                    , "Content-Type: text/plain\r\n"
                    , "Foo: Bar\r\n"
                    , "Transfer-Encoding: chunked\r\n\r\n"
                    , "a\r\n"
                    , "0123456789\r\n"
                    , "0\r\n\r\n"
                    ]


  where
    rsp1 = updateHeaders (Map.insert "Foo" ["Bar"]) $
           setContentLength 10 $
           setResponseStatus 600 "Test" $
           modifyResponseBody (>==> (enumBS "0123456789")) $
           setResponseBody returnI $
           emptyResponse { rspHttpVersion = (1,0) }
    rsp2 = rsp1 { rspContentLength = Nothing }
    rsp3 = setContentType "text/plain" $ (rsp2 { rspHttpVersion = (1,1) })


testHttpResponse4 :: Test
testHttpResponse4 = testCase "server/HttpResponse4" $ do
    sstep <- runIteratee copyingStream2Stream

    req <- mkRequest sampleRequest

    b <- run_ $ rsm $
         sendResponse req rsp1 sstep testOnSendFile >>=
                      return . snd

    assertEqual "http response" (L.concat [
                      "HTTP/1.0 304 Test\r\n"
                    , "Content-Length: 0\r\n\r\n"
                    ]) b

  where
    rsp1 = setResponseStatus 304 "Test" $
           emptyResponse { rspHttpVersion = (1,0) }


-- httpServe "127.0.0.1" 8080 "localhost" pongServer



echoServer :: (ByteString -> IO ())
           -> Request
           -> Iteratee ByteString IO (Request,Response)
echoServer _ req = do
    se <- liftIO $ readIORef (rqBody req)
    let (SomeEnumerator enum) = se
    i <- liftM enum $ lift $ runIteratee copyingStream2Stream
    b <- i
    let cl = L.length b
    liftIO $ writeIORef (rqBody req) (SomeEnumerator $ joinI . I.take 0)
    return (req, rsp b cl)
  where
    rsp s cl = emptyResponse { rspBody = Enum $ enumLBS s
                             , rspContentLength = Just $ fromIntegral cl }


echoServer2 :: ServerHandler
echoServer2 _ req = do
    (rq,rsp) <- echoServer (const $ return ()) req
    return (rq, addCookie cook rsp)
  where
    cook = Cookie "foo" "bar" (Just utc) (Just ".foo.com") (Just "/")
    utc = UTCTime (ModifiedJulianDay 55226) 0


testHttp1 :: Test
testHttp1 = testCase "server/httpSession" $ do
    let enumBody = enumBS sampleRequest >==> enumBS sampleRequest2

    ref <- newIORef ""

    let (iter,onSendFile) = mkIter ref

    runHTTP Nothing Nothing echoServer "localhost" (SessionInfo "127.0.0.1" 80 "127.0.0.1" 58384 False)
            enumBody iter onSendFile (return ())

    s <- readIORef ref

    let lns = LC.lines s

    let ok = case lns of
               ([ "HTTP/1.1 200 OK\r"
                , "Content-Length: 10\r"
                , d1
                , s1
                , "\r"
                , "0123456789HTTP/1.1 200 OK\r"
                , "Content-Length: 14\r"
                , d2
                , s2
                , "\r"
                , "01234567890123" ]) -> (("Date" `L.isPrefixOf` d1) &&
                                          ("Date" `L.isPrefixOf` d2) &&
                                          ("Server" `L.isPrefixOf` s1) &&
                                          ("Server" `L.isPrefixOf` s2))

               _ -> False

    when (not ok) $ do
        putStrLn "server/httpSession fail!!!! got:"
        LC.putStrLn s

    assertBool "pipelined responses" ok


mkIter :: IORef L.ByteString
       -> (Iteratee ByteString IO (), FilePath -> Int64 -> Int64 -> IO ())
mkIter ref = (iter, \f st sz -> onF f st sz iter)
  where
    iter = do
        x <- copyingStream2Stream
        liftIO $ modifyIORef ref $ \s -> L.append s x

    onF f st sz i = do
        step <- runIteratee i
        let it = enumFilePartial f (st,st+sz) step
        run_ it


testChunkOn1_0 :: Test
testChunkOn1_0 = testCase "server/transfer-encoding chunked" $ do
    let enumBody = enumBS sampleRequest1_0

    ref <- newIORef ""
    let (iter,onSendFile) = mkIter ref

    done <- newEmptyMVar
    forkIO (runHTTP Nothing Nothing f "localhost" (SessionInfo "127.0.0.1" 80 "127.0.0.1" 58384 False)
                enumBody iter onSendFile (return ())
            `finally` putMVar done ())

    takeMVar done

    -- this is a pretty lame way of checking whether the output was chunked,
    -- but "whatever"
    output <- liftM lower $ readIORef ref

    assertBool "chunked output" $ not $ S.isInfixOf "chunked" output
    assertBool "connection close" $ S.isInfixOf "connection: close" output

  where
    lower = S.map toLower . S.concat . L.toChunks

    f :: ServerHandler
    f _ req = do
        let s = L.fromChunks $ Prelude.take 500 $ repeat "fldkjlfksdjlfd"
        let out = enumLBS s
        return (req, emptyResponse { rspBody = Enum out })


sampleRequest4 :: ByteString
sampleRequest4 =
    S.concat [ "\r\nGET /foo/bar.html?param1=abc&param2=def%20+&param1=abc HTTP/1.1\r\n"
             , "Host: www.zabble.com:7777\r\n"
             , "Content-Length: 10\r\n"
             , "Connection: close\r\n"
             , "X-Random-Other-Header: foo\r\n bar\r\n"
             , "Cookie: foo=\"bar\\\"\"\r\n"
             , "\r\n"
             , "0123456789" ]


testHttp2 :: Test
testHttp2 = testCase "server/connection: close" $ do
    let enumBody = enumBS sampleRequest4 >==> enumBS sampleRequest2

    ref <- newIORef ""

    let (iter,onSendFile) = mkIter ref

    done <- newEmptyMVar

    forkIO (runHTTP Nothing
                    Nothing
                    echoServer2
                    "localhost"
                    (SessionInfo "127.0.0.1" 80 "127.0.0.1" 58384 False)
                    enumBody
                    iter
                    onSendFile
                    (return ()) `finally` putMVar done ())

    takeMVar done

    s <- readIORef ref

    let lns = LC.lines s

    let ok = case lns of
               ([ "HTTP/1.1 200 OK\r"
                , "Connection: close\r"
                , "Content-Length: 10\r"
                , d1
                , s1
                , "Set-Cookie: foo=bar; path=/; expires=Sat, 30-Jan-2010 00:00:00 GMT; domain=.foo.com\r"
                , "\r"
                , "0123456789" ]) -> (("Date" `L.isPrefixOf` d1) &&
                                      ("Server" `L.isPrefixOf` s1))

               _ -> False

    assertBool "connection: close" ok



testHttp100 :: Test
testHttp100 = testCase "server/expect100" $ do
    let enumBody = enumBS sampleRequestExpectContinue

    ref <- newIORef ""

    let (iter,onSendFile) = mkIter ref

    runHTTP Nothing
            Nothing
            echoServer2
            "localhost"
            (SessionInfo "127.0.0.1" 80 "127.0.0.1" 58384 False)
            enumBody
            iter
            onSendFile
            (return ())

    s <- readIORef ref

    let lns = LC.lines s

    let ok = case lns of
               ([ "HTTP/1.1 100 Continue\r"
                , "\r"
                , "HTTP/1.1 200 OK\r"
                , "Content-Length: 10\r"
                , d1
                , s1
                , "Set-Cookie: foo=bar; path=/; expires=Sat, 30-Jan-2010 00:00:00 GMT; domain=.foo.com\r"
                , "\r"
                , "0123456789" ]) -> (("Date" `L.isPrefixOf` d1) &&
                                      ("Server" `L.isPrefixOf` s1))

               _ -> False

    when (not ok) $ do
        putStrLn "expect100 fail! got:"
        LC.putStrLn s

    assertBool "100 Continue" ok


testExpectGarbage :: Test
testExpectGarbage = testCase "server/Expect: garbage" $ do
    let enumBody = enumBS sampleRequestExpectGarbage

    ref <- newIORef ""

    let (iter,onSendFile) = mkIter ref

    runHTTP Nothing
            Nothing
            echoServer2
            "localhost"
            (SessionInfo "127.0.0.1" 80 "127.0.0.1" 58384 False)
            enumBody
            iter
            onSendFile
            (return ())

    s <- readIORef ref

    let lns = LC.lines s

    let ok = case lns of
               ([ "HTTP/1.1 200 OK\r"
                , "Content-Length: 10\r"
                , d1
                , s1
                , "Set-Cookie: foo=bar; path=/; expires=Sat, 30-Jan-2010 00:00:00 GMT; domain=.foo.com\r"
                , "\r"
                , "0123456789" ]) -> (("Date" `L.isPrefixOf` d1) &&
                                      ("Server" `L.isPrefixOf` s1))

               _ -> False

    assertBool "random expect: header" ok




pongServer :: Snap ()
pongServer = modifyResponse $ setResponseBody (enumBS "PONG") .
                              setContentType "text/plain" .
                              setContentLength 4

sendFileFoo :: Snap ()
sendFileFoo = sendFile "data/fileServe/foo.html"


testSendFile :: Test
testSendFile = testCase "server/sendFile" $ do
    bracket (forkIO $ httpServe [HttpPort "*" port] Nothing "localhost"
                                Nothing Nothing
                    $ runSnap sendFileFoo)
            (killThread)
            (\tid -> do
                 m <- timeout (120 * seconds) $ go tid
                 maybe (assertFailure "timeout")
                       (const $ return ())
                       m)

  where
    go tid = do
        waitabit

        rsp <- HTTP.simpleHTTP (HTTP.getRequest "http://localhost:8123/")
        doc <- HTTP.getResponseBody rsp

        killThread tid
        waitabit

        assertEqual "sendFile" "FOO\n" doc


    waitabit = threadDelay $ ((10::Int)^(6::Int))

    port     = 8123


testServerStartupShutdown :: Test
testServerStartupShutdown = testCase "server/startup/shutdown" $ do
    bracket (forkIO $
             httpServe [HttpPort "*" port]
                       Nothing
                       "localhost"
                       (Just "test-access.log")
                       (Just "test-error.log")
                       (runSnap pongServer))
            (killThread)
            (\tid -> do
                 m <- timeout (120 * seconds) $ go tid
                 maybe (assertFailure "timeout")
                       (const $ return ())
                       m)


  where
    go tid = do
        debug $ "testServerStartupShutdown: waiting a bit"
        waitabit
        debug $ "testServerStartupShutdown: sending http request"
        rsp <- HTTP.simpleHTTP (HTTP.getRequest "http://localhost:8145/")
        debug $ "testServerStartupShutdown: grabbing response"
        doc <- HTTP.getResponseBody rsp
        assertEqual "server" "PONG" doc

        debug $ "testServerStartupShutdown: killing thread"
        killThread tid
        debug $ "testServerStartupShutdown: kill signal sent to thread"
        waitabit

        expectException $ HTTP.simpleHTTP
                        $ HTTP.getRequest "http://localhost:8145/"
        return ()

    waitabit = threadDelay $ 2*((10::Int)^(6::Int))

    port = 8145


testServerShutdownWithOpenConns :: Test
testServerShutdownWithOpenConns = testCase "server/shutdown-open-conns" $ do
    tid <- forkIO $
           httpServe [HttpPort "127.0.0.1" port]
                     Nothing
                     "localhost"
                     Nothing
                     Nothing
                     (runSnap pongServer)

    waitabit

    result <- newEmptyMVar

    forkIO $ do
        e <- try $ withSock port $ \sock -> do
                 N.sendAll sock "GET /"
                 waitabit
                 killThread tid
                 waitabit
                 N.sendAll sock "pong HTTP/1.1\r\n"
                 N.sendAll sock "Host: 127.0.0.1\r\n"
                 N.sendAll sock "Content-Length: 0\r\n"
                 N.sendAll sock "Connection: close\r\n\r\n"

                 resp <- recvAll sock
                 when (S.null resp) $ throwIO TestException

                 let s = S.unpack $ Prelude.head $ ditchHeaders $ S.lines resp
                 debug $ "got HTTP response " ++ s ++ ", we shouldn't be here...."

        putMVar result e

    e <- timeout (75*seconds) $ takeMVar result

    case e of
      Nothing  -> killThread tid >> assertFailure "timeout"
      (Just r) ->
          case r of
            (Left (_::SomeException)) -> return ()
            (Right _)                 -> assertFailure "socket didn't get killed"


  where
    waitabit = threadDelay $ 2*((10::Int)^(6::Int))
    port = 8146



seconds :: Int
seconds = (10::Int) ^ (6::Int)


copyingStream2Stream :: (Monad m) => Iteratee ByteString m L.ByteString
copyingStream2Stream = go []
  where
    go l = do
        mbx <- I.head
        maybe (return $ L.fromChunks $ reverse l)
              (\x -> let !z = S.copy x in go (z:l))
              mbx
