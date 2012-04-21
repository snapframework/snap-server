{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}

module Test.Blackbox
  ( tests
  , ssltests
  , startTestServer ) where

--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Concurrent
import           Control.Exception (SomeException, catch, throwIO)
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.CaseInsensitive (CI)
import           Data.Int
import           Data.List
import           Data.Monoid
import qualified Network.HTTP.Enumerator as HTTP
import qualified Network.Socket.ByteString as N
import           Network.TLS (TLSCertificateUsage(..))
import           Prelude hiding (catch, take)
import           System.Timeout
import           Test.Framework
import           Test.Framework.Options
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test, path)
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as QC
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic hiding (run, assert)
------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Iteratee hiding (map, head)
import qualified Snap.Iteratee as I
import           Snap.Http.Server
import           Snap.Test.Common
import           Test.Common.Rot13
import           Test.Common.TestHandler
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
                , testFileUpload
                , testTimeoutTickle
                , testTimeoutBadTickle
                ]


------------------------------------------------------------------------------
tests :: Int -> [Test]
tests port = map (\f -> f False port "") testFunctions


------------------------------------------------------------------------------
slowTestOptions :: Bool -> TestOptions' Maybe
slowTestOptions ssl =
    if ssl
      then mempty { topt_maximum_generated_tests = Just 75 }
      else mempty { topt_maximum_generated_tests = Just 300 }


------------------------------------------------------------------------------
ssltests :: Maybe Int -> [Test]
ssltests = maybe [] httpsTests
    where httpsTests port = map (\f -> f True port sslname) testFunctions
          sslname = "ssl/"

------------------------------------------------------------------------------
startTestServer :: Int
                -> Maybe Int
                -> IO (ThreadId, MVar ())
startTestServer port sslport = do
    let cfg = setAccessLog      (ConfigFileLog "ts-access.log") .
              setErrorLog       (ConfigFileLog "ts-error.log")  .
              setBind           "*"                             .
              setPort           port                            .
              setDefaultTimeout 10                              .
              setVerbose        False                           $
              defaultConfig

    let cfg' = case sslport of
                Nothing -> cfg
                Just p  -> setSSLPort p                     .
                           setSSLBind "*"                   .
                           setSSLCert "cert.pem"            .
                           setSSLKey  "key.pem"             .
                           setAccessLog "ts-access-ssl.log" .
                           setErrorLog "ts-error-ssl.log"   $
                           cfg

    mvar <- newEmptyMVar
    tid  <- forkIO $ do
                (httpServe cfg' testHandler)
                  `catch` \(_::SomeException) -> return ()
                putMVar mvar ()
    threadDelay $ 4*seconds

    return (tid,mvar)


------------------------------------------------------------------------------
doPong :: Bool -> Int -> IO ByteString
doPong ssl port = do
    debug "getting URI"
    let !uri = (if ssl then "https" else "http")
               ++ "://127.0.0.1:" ++ show port ++ "/pong"
    debug $ "URI is: '" ++ uri ++ "', calling simpleHttp"

    rsp <- fetch uri

    debug $ "response was " ++ show rsp
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
testPong ssl port name = testCase (name ++ "blackbox/pong") $ do
    doc <- doPong ssl port
    assertEqual "pong response" "PONG" doc


------------------------------------------------------------------------------
-- FIXME: waiting on http-enumerator patch for HEAD behaviour
-- testHeadPong :: Bool -> Int -> String -> Test
-- testHeadPong ssl port name = testCase (name ++ "blackbox/pong/HEAD") $ do
--     doc <- headPong ssl port
--     assertEqual "pong HEAD response" "" doc


------------------------------------------------------------------------------
testEcho :: Bool -> Int -> String -> Test
testEcho ssl port name =
    plusTestOptions (slowTestOptions ssl) $
    testProperty (name ++ "blackbox/echo") $
    QC.mapSize (if ssl then min 100 else min 300) $
    monadicIO $ forAllM arbitrary prop
  where
    prop txt = do
        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/echo"

        doc <- QC.run $ post uri txt []
        QC.assert $ txt == doc


------------------------------------------------------------------------------
testFileUpload :: Bool -> Int -> String -> Test
testFileUpload ssl port name =
    plusTestOptions (slowTestOptions ssl) $
    testProperty (name ++ "blackbox/upload") $
    QC.mapSize (if ssl then min 100 else min 300) $
    monadicIO $
    forAllM arbitrary prop
  where
    boundary = "boundary-jdsklfjdsalkfjadlskfjldskjfldskjfdsfjdsklfldksajfl"

    prefix = [ "--"
             , boundary
             , "\r\n"
             , "content-disposition: form-data; name=\"submit\"\r\n"
             , "\r\nSubmit\r\n" ]

    body kvps = L.concat $ prefix ++ concatMap part kvps ++ suffix
      where
        part (k,v) = [ "--"
                     , boundary
                     , "\r\ncontent-disposition: attachment; filename=\""
                     , k
                     , "\"\r\nContent-Type: text/plain\r\n\r\n"
                     , v
                     , "\r\n" ]

    suffix = [ "--", boundary, "--\r\n" ]

    hdrs = [ ("Content-type", S.concat $ [ "multipart/form-data; boundary=" ]
                                         ++ L.toChunks boundary) ]

    b16 (k,v) = (ne $ e k, e v)
      where
        ne s = if L.null s then "file" else s
        e s = L.fromChunks [ B16.encode $ S.concat $ L.toChunks s ]

    response kvps = L.concat $ [ "Param:\n"
                               , "submit\n"
                               , "Value:\n"
                               , "Submit\n\n" ] ++ concatMap responseKVP kvps

    responseKVP (k,v) = [ "File:\n"
                        , k
                        , "\nValue:\n"
                        , v
                        , "\n\n" ]

    prop kvps' = do
        let kvps = sort $ map b16 kvps'

        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/upload/handle"

        let txt = response kvps
        doc <- QC.run $ post uri (body kvps) hdrs

        when (txt /= doc) $ QC.run $ do
                     L.putStrLn "expected:"
                     L.putStrLn "----------------------------------------"
                     L.putStrLn txt
                     L.putStrLn "----------------------------------------"
                     L.putStrLn "\ngot:"
                     L.putStrLn "----------------------------------------"
                     L.putStrLn doc
                     L.putStrLn "----------------------------------------"

        QC.assert $ txt == doc


------------------------------------------------------------------------------
testRot13 :: Bool -> Int -> String -> Test
testRot13 ssl port name =
    plusTestOptions (slowTestOptions ssl) $
    testProperty (name ++ "blackbox/rot13") $
    monadicIO $ forAllM arbitrary prop
  where
    prop txt = do
        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/rot13"

        doc <- QC.run $ liftM (S.concat . L.toChunks)
                      $ post uri (L.fromChunks [txt]) []
        QC.assert $ txt == rot13 doc


------------------------------------------------------------------------------
-- TODO: this one doesn't work w/ SSL
testSlowLoris :: Bool -> Int -> String -> Test
testSlowLoris ssl port name = testCase (name ++ "blackbox/slowloris") $
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
    testCase (name ++ "blackbox/testBlockingRead") $
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
    testCase (name ++ "blackbox/testPartial") $
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
    testCase (name ++ "blackbox/testBigResponse") $
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


------------------------------------------------------------------------------
parseURL :: String -> IO (HTTP.Request IO)
parseURL url = do
    req <- HTTP.parseUrl url
    return $ req { HTTP.checkCerts = const $ const $
                                     return CertificateUsageAccept }


------------------------------------------------------------------------------
fetchReq :: HTTP.Request IO -> IO (L.ByteString)
fetchReq req = go `catch` (\(e::SomeException) -> do
                 debug $ "simpleHttp threw exception: " ++ show e
                 throwIO e)
  where
    go = do
        rsp <- HTTP.withManager $ HTTP.httpLbs req
        return $ HTTP.responseBody rsp


------------------------------------------------------------------------------
fetch :: String -> IO (L.ByteString)
fetch url = do
    req <- parseURL url `catch` (\(e::SomeException) -> do
                 debug $ "parseURL threw exception: " ++ show e
                 throwIO e)
    fetchReq req


------------------------------------------------------------------------------
post :: String
     -> L.ByteString
     -> [(CI ByteString, ByteString)]
     -> IO (L.ByteString)
post url body hdrs = do
    req <- parseURL url `catch` (\(e::SomeException) -> do
                 debug $ "parseURL threw exception: " ++ show e
                 throwIO e)
    fetchReq $ req { HTTP.requestBody    = HTTP.RequestBodyLBS body
                   , HTTP.method         = "POST"
                   , HTTP.requestHeaders = hdrs }


------------------------------------------------------------------------------
-- This test checks two things:
--
-- 1. that the timeout tickling logic works
-- 2. that "flush" is passed along through a gzip operation.
testTimeoutTickle :: Bool -> Int -> String -> Test
testTimeoutTickle ssl port name =
    testCase (name ++ "blackbox/timeout/tickle") $ do
        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/timeout/tickle"
        doc <- liftM (S.concat . L.toChunks) $ fetch uri
        let expected = S.concat $ replicate 10 ".\n"
        assertEqual "response equal" expected doc


------------------------------------------------------------------------------
testTimeoutBadTickle :: Bool -> Int -> String -> Test
testTimeoutBadTickle ssl port name =
    testCase (name ++ "blackbox/timeout/badtickle") $ do
        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/timeout/badtickle"
        expectException $ fetch uri
