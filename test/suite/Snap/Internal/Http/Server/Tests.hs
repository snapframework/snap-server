{-# LANGUAGE OverloadedStrings #-}
module Snap.Internal.Http.Server.Tests
  ( tests ) where

import           Control.Exception (try, SomeException)
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w)
import           Data.IORef
import qualified Data.Map as Map
import           Data.Time.Calendar
import           Data.Time.Clock
import           Prelude hiding (take)
import           Test.Framework 
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)


import           Snap.Internal.Http.Types
import           Snap.Internal.Http.Server
import           Snap.Iteratee


tests :: [Test]
tests = [ testHttpRequest1
        , testMultiRequest
        , testHttpRequest2
        , testHttpRequest3
        , testHttpResponse1
        , testHttp1
        , testHttp2
        , testPartialParse
        , testMethodParsing ]


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
             , "Set-Cookie: foo=\"bar\\\"\"\r\n"
             , "\r\n"
             , "0123456789" ]

testMethodParsing :: Test
testMethodParsing =
    testCase "method parsing" $ mapM_ testOneMethod ms
  where
    ms = [ GET, HEAD, POST, PUT, DELETE, TRACE, OPTIONS, CONNECT ]

testHttpRequest1 :: Test
testHttpRequest1 =
    testCase "HttpRequest1" $ do
        iter <- enumBS sampleRequest $
                do
                    r <- rsm receiveRequest
                    b <- liftM fromWrap $ joinIM $ rqBody r stream2stream
                    return (r,b)

        (req,body) <- run iter

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
                    rqParam req "param1"
        assertEqual "param2" (Just ["def  "]) $
                    rqParam req "param2"



testMultiRequest :: Test
testMultiRequest =
    testCase "MultiRequest" $ do
        iter <- (enumBS sampleRequest >. enumBS sampleRequest) $
                do
                    r1 <- rsm receiveRequest
                    b1 <- liftM fromWrap $ joinIM $ rqBody r1 stream2stream
                    r2 <- rsm receiveRequest
                    b2 <- liftM fromWrap $ joinIM $ rqBody r2 stream2stream
                    return (r1,b1,r2,b2)

        (req1,body1,req2,body2) <- run iter

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
    iter <- enumLBS txt $ rsm receiveRequest
    req <- run iter

    assertEqual "method" m $ rqMethod req

  where
    txt = methodTestText m


sampleShortRequest :: ByteString
sampleShortRequest = "GET /fo"

testPartialParse :: Test
testPartialParse = testCase "Short" $ do
    iter <- enumBS sampleShortRequest $ rsm receiveRequest

    e <- (try $ run iter) :: IO (Either SomeException Request)

    case e of (Left _) -> return ()
              Right x  -> assertFailure $ "expected exception, got " ++ show x


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
    testCase "HttpRequest2" $ do
        iter <- enumBS sampleRequest2 $
                do
                    r <- rsm receiveRequest
                    b <- liftM fromWrap $ joinIM $ rqBody r stream2stream
                    return (r,b)

        (_,body) <- run iter

        assertEqual "parse body" "01234567890123" body


testHttpRequest3 :: Test
testHttpRequest3 =
    testCase "HttpRequest3" $ do
        iter <- enumBS sampleRequest3 $
                do
                    r <- rsm receiveRequest
                    b <- liftM fromWrap $ joinIM $ rqBody r stream2stream
                    return (r,b)

        (req,body) <- run iter

        assertEqual "no cookies" [] $ rqCookies req

        assertEqual "multiheader" (Just ["1","2"]) $
                    Map.lookup "Multiheader" (rqHeaders req)

        assertEqual "host" ("localhost", 80) $
                    (rqServerName req, rqServerPort req)

        assertEqual "post param 1"
                    (rqParam req "postparam1")
                    (Just ["1"])

        assertEqual "post param 2"
                    (rqParam req "postparam2")
                    (Just ["2"])

        -- if we're www-form-encoded then we will have read the body already
        assertEqual "parse body" "" body


sampleRequest3 :: ByteString
sampleRequest3 =
    S.concat [ "\r\nGET /foo/bar.html?param1=abc&param2=def%20+&param1=abc HTTP/1.1\r\n"
             , "Content-Type: application/x-www-form-urlencoded\r\n"
             , "Content-Length: 25\r\n"
             , "Multiheader: 1\r\n"
             , "Multiheader: 2\r\n"
             , "X-Random-Other-Header: foo\r\n bar\r\n"
             , "\r\n"
             , "postparam1=1&postparam2=2" ]


rsm :: ServerMonad a -> Iteratee IO a
rsm = runServerMonad "localhost" "127.0.0.1" 80 "127.0.0.1" 58382


testHttpResponse1 :: Test
testHttpResponse1 = testCase "HttpResponse1" $ do
    b <- run $ rsm $
         sendResponse rsp1 stream2stream >>= return . fromWrap

    assertEqual "http response" b $ L.concat [
                      "HTTP/1.0 600 Test\r\n"
                    , "Content-Length: 10\r\n"
                    , "Foo: Bar\r\n\r\n"
                    , "0123456789"
                    ]

    b2 <- run $ rsm $
          sendResponse rsp2 stream2stream >>= return . fromWrap

    assertEqual "http response" b2 $ L.concat [
                      "HTTP/1.0 600 Test\r\n"
                    , "Foo: Bar\r\n\r\n"
                    , "0123456789"
                    ]

    b3 <- run $ rsm $
          sendResponse rsp3 stream2stream >>= return . fromWrap

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
           filterResponseBody (>. (enumBS "0123456789")) $
           setResponseBody return $
           emptyResponse { rspHttpVersion = (1,0) }

    rsp2 = rsp1 { rspContentLength = Nothing }
    rsp3 = setContentType "text/plain" $ (rsp2 { rspHttpVersion = (1,1) })


-- httpServe "127.0.0.1" 8080 "localhost" pongServer 



echoServer :: Request -> Iteratee IO (Request,Response)
echoServer req = do
    b <- liftM fromWrap $ joinIM $ rqBody req stream2stream
    let cl = L.length b
    let enum i = return $ joinI $ take 0 i
    return (req {rqBody=enum}, rsp b cl)
  where
    rsp s cl = emptyResponse { rspBody = enumLBS s
                             , rspContentLength = Just $ fromIntegral cl }


echoServer2 :: Request -> Iteratee IO (Request,Response)
echoServer2 req = do
    (rq,rsp) <- echoServer req
    return (rq, addCookie cook rsp)
  where
    cook = Cookie "foo" "bar" (Just utc) (Just ".foo.com") (Just "/")
    utc = UTCTime (ModifiedJulianDay 55226) 0


testHttp1 :: Test
testHttp1 = testCase "http session" $ do
    let enumBody = enumBS sampleRequest >. enumBS sampleRequest2

    ref <- newIORef ""

    let iter = mkIter ref

    runHTTP "localhost" "127.0.0.1" 80 "127.0.0.1" 58384
            enumBody iter echoServer

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

    assertBool "pipelined responses" ok


  where
    mkIter :: IORef L.ByteString -> Iteratee IO ()
    mkIter ref = do
        x <- stream2stream
        liftIO $ modifyIORef ref $ \s -> L.append s (fromWrap x)


sampleRequest4 :: ByteString
sampleRequest4 =
    S.concat [ "\r\nGET /foo/bar.html?param1=abc&param2=def%20+&param1=abc HTTP/1.1\r\n"
             , "Host: www.zabble.com:7777\r\n"
             , "Content-Length: 10\r\n"
             , "Connection: close\r\n"
             , "X-Random-Other-Header: foo\r\n bar\r\n"
             , "Set-Cookie: foo=\"bar\\\"\"\r\n"
             , "\r\n"
             , "0123456789" ]

testHttp2 :: Test
testHttp2 = testCase "connection: close" $ do
    let enumBody = enumBS sampleRequest4 >. enumBS sampleRequest2

    ref <- newIORef ""

    let iter = mkIter ref

    runHTTP "localhost" "127.0.0.1" 80 "127.0.0.1" 58384
            enumBody iter echoServer2

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

    assertBool "connection: close" ok


  where
    mkIter :: IORef L.ByteString -> Iteratee IO ()
    mkIter ref = do
        x <- stream2stream
        liftIO $ modifyIORef ref $ \s -> L.append s (fromWrap x)
