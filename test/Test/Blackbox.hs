{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Blackbox
  ( tests
  , ssltests
  , startTestServers
  ) where

--------------------------------------------------------------------------------
import           Control.Concurrent                   (ThreadId, forkIO, threadDelay)
import           Control.Exception                    (bracketOnError)
import           Control.Monad                        (Monad (return), forever, liftM, mapM_, when)
import qualified Data.ByteString.Base16               as B16
import           Data.ByteString.Char8                (ByteString)
import qualified Data.ByteString.Char8                as S
import qualified Data.ByteString.Lazy.Char8           as L
import           Data.Int                             (Int)
import           Data.List                            (concat, concatMap, head, map, replicate, sort, (++))
import           Data.Monoid                          (Monoid (mconcat, mempty))
import qualified Network.Http.Client                  as HTTP
import qualified Network.Socket                       as N
import qualified Network.Socket.ByteString            as N
import           Prelude                              hiding (catch, take)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder             (fromByteString)
#ifdef OPENSSL
import qualified OpenSSL.Session                      as SSL
#endif
import qualified System.IO.Streams                    as Streams
import           System.Timeout                       (timeout)
import           Test.Framework                       (Test, TestOptions' (topt_maximum_generated_tests), plusTestOptions)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           hiding (Test, path)
import           Test.QuickCheck                      (Arbitrary (arbitrary))
import           Test.QuickCheck.Monadic              (forAllM, monadicIO)
import qualified Test.QuickCheck.Monadic              as QC
import qualified Test.QuickCheck.Property             as QC
------------------------------------------------------------------------------
import           Snap.Internal.Debug                  (debug)
import           Snap.Internal.Http.Server.Session    (httpAcceptLoop, snapToServerHandler)
import qualified Snap.Internal.Http.Server.Socket     as Sock
import qualified Snap.Internal.Http.Server.Types      as Types
import           Snap.Test.Common                     (ditchHeaders, expectExceptionBeforeTimeout, recvAll, withSock)
import           Test.Common.Rot13                    (rot13)
import           Test.Common.TestHandler              (testHandler)


------------------------------------------------------------------------------
tests :: Int -> [Test]
tests port = map (\f -> f False port "") testFunctions

ssltests :: Maybe Int -> [Test]
ssltests = maybe [] httpsTests
    where httpsTests port = map (\f -> f True port sslname) testFunctions
          sslname = "ssl/"

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
                , testServerHeader
                , testFileServe
                ]


------------------------------------------------------------------------------
-- | Returns the thread the server is running in as well as the port it is
-- listening on.
startTestSocketServer :: IO (ThreadId, Int)
startTestSocketServer = bracketOnError getSock cleanup forkServer
  where
    getSock = do
#ifdef OPENSSL
        HTTP.modifyContextSSL $ \ctx -> do
            SSL.contextSetVerificationMode ctx SSL.VerifyNone
            return ctx
#endif
        Sock.bindHttp "127.0.0.1" (fromIntegral N.aNY_PORT)

    forkServer sock = do
        port <- liftM fromIntegral $ N.socketPort sock
        tid <- forkIO $ httpAcceptLoop (snapToServerHandler testHandler)
                                       emptyServerConfig
                                       (Sock.httpAcceptFunc sock)
        return (tid, port)

    cleanup = N.close

    logAccess !_ !_ !_             = return ()
    logError !_                    = return ()
    onStart !_                     = return ()
    onParse !_ !_                  = return ()
    onUserHandlerFinished !_ !_ !_ = return ()
    onDataFinished !_ !_ !_        = return ()
    onExceptionHook !_ !_          = return ()
    onEscape !_                    = return ()

    emptyServerConfig = Types.ServerConfig logAccess
                                           logError
                                           onStart
                                           onParse
                                           onUserHandlerFinished
                                           onDataFinished
                                           onExceptionHook
                                           onEscape
                                           "localhost"
                                           6
                                           False
                                           1


------------------------------------------------------------------------------
waitabit :: IO ()
waitabit = threadDelay $ 2*seconds


------------------------------------------------------------------------------
seconds :: Int
seconds = (10::Int) ^ (6::Int)


------------------------------------------------------------------------------
fetch :: ByteString -> IO ByteString
fetch url = HTTP.get url HTTP.concatHandler'


------------------------------------------------------------------------------
slowTestOptions :: Bool -> TestOptions' Maybe
slowTestOptions ssl =
    if ssl
      then mempty { topt_maximum_generated_tests = Just 75 }
      else mempty { topt_maximum_generated_tests = Just 300 }


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
-- FIXME: waiting on http-enumerator patch for HEAD behaviour
-- testHeadPong :: Bool -> Int -> String -> Test
-- testHeadPong ssl port name = testCase (name ++ "blackbox/pong/HEAD") $ do
--     doc <- headPong ssl port
--     assertEqual "pong HEAD response" "" doc


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
-- TODO: this one doesn't work w/ SSL
testSlowLoris :: Bool -> Int -> String -> Test
testSlowLoris ssl port name = testCase (name ++ "blackbox/slowloris") $
                              if ssl then return () else withSock port go

  where
    go sock = do
        N.sendAll sock "POST /echo HTTP/1.1\r\n"
        N.sendAll sock "Host: 127.0.0.1\r\n"
        N.sendAll sock "Content-Length: 2500000\r\n"
        N.sendAll sock "Connection: close\r\n\r\n"

        b <- expectExceptionBeforeTimeout (loris sock) 30

        assertBool "didn't catch slow loris attack" b

    loris sock = forever $ do
        N.sendAll sock "."
        waitabit


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

        doc <- QC.run $ HTTP.post (S.pack uri) "text/plain"
                                  (Streams.write (Just $ fromByteString txt))
                                  HTTP.concatHandler'
        QC.assert $ txt == rot13 doc


------------------------------------------------------------------------------
doPong :: Bool -> Int -> IO ByteString
doPong ssl port = do
    debug "getting URI"
    let !uri = (if ssl then "https" else "http")
               ++ "://127.0.0.1:" ++ show port ++ "/pong"
    debug $ "URI is: '" ++ uri ++ "', calling simpleHttp"

    rsp <- fetch $ S.pack uri

    debug $ "response was " ++ show rsp
    return rsp


------------------------------------------------------------------------------
testPong :: Bool -> Int -> String -> Test
testPong ssl port name = testCase (name ++ "blackbox/pong") $ do
    doc <- doPong ssl port
    assertEqual "pong response" "PONG" doc


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
-- This test checks two things:
--
-- 1. that the timeout tickling logic works
-- 2. that "flush" is passed along through a gzip operation.
testTimeoutTickle :: Bool -> Int -> String -> Test
testTimeoutTickle ssl port name =
    testCase (name ++ "blackbox/timeout/tickle") $ do
        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/timeout/tickle"
        doc <- fetch $ S.pack uri
        let expected = S.concat $ replicate 10 ".\n"
        assertEqual "response equal" expected doc


------------------------------------------------------------------------------
testFileServe :: Bool -> Int -> String -> Test
testFileServe ssl port name =
    testCase (name ++ "blackbox/fileserve") $ do
        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/fileserve/hello.txt"
        doc <- fetch $ S.pack uri
        let expected = "hello world\n"
        assertEqual "response equal" expected doc


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

        let uri = S.pack $ concat [ if ssl then "https" else "http"
                                  , "://127.0.0.1:"
                                  , show port
                                  , "/upload/handle" ]

        let txt = response kvps
        doc0 <- QC.run
                  $ HTTP.withConnection (HTTP.establishConnection uri)
                  $ \conn -> do
            req <- HTTP.buildRequest $ do
                HTTP.http HTTP.POST uri
                mapM_ (uncurry HTTP.setHeader) hdrs

            HTTP.sendRequest conn req (Streams.write $ Just
                                                     $ mconcat
                                                     $ map fromByteString
                                                     $ L.toChunks
                                                     $ body kvps)
            HTTP.receiveResponse conn HTTP.concatHandler'

        let doc = L.fromChunks [doc0]
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

        doc <- QC.run $ HTTP.post (S.pack uri) "text/plain"
                                  (Streams.write (Just $ fromByteString txt))
                                  HTTP.concatHandler'
        QC.assert $ txt == doc


------------------------------------------------------------------------------
testServerHeader :: Bool -> Int -> String -> Test
testServerHeader ssl port name =
    testCase (name ++ "blackbox/server-header") $ do
        let uri = (if ssl then "https" else "http")
                  ++ "://127.0.0.1:" ++ show port ++ "/server-header"
        HTTP.get (S.pack uri) $ \resp _ -> do
            let serverHeader = HTTP.getHeader resp "server"
            assertEqual "server header" (Just "foo") serverHeader


------------------------------------------------------------------------------
startTestServers :: Bool -> IO ((ThreadId, Int), Maybe (ThreadId, Int))
startTestServers _ = do
    x <- startTestSocketServer
    return (x, Nothing)

{-
    let cfg = setAccessLog      (ConfigFileLog "ts-access.log") .
              setErrorLog       (ConfigFileLog "ts-error.log")  .
              setBind           "*"                             .
              setPort           port                            .
              setDefaultTimeout 10                              .
              setVerbose        False                           $
              defaultConfig

    let cfg' = maybe cfg
                     (\p ->
                      setSSLPort   p                                   .
                      setSSLBind   "*"                                 .
                      setSSLCert   "cert.pem"                          .
                      setSSLKey    "key.pem"                           .
                      setAccessLog (ConfigFileLog "ts-access-ssl.log") .
                      setErrorLog  (ConfigFileLog "ts-error-ssl.log")  $
                      cfg)
                     sslport

    mvar <- newEmptyMVar
    tid  <- forkIO $ do
                (httpServe cfg' testHandler)
                  `catch` \(_::SomeException) -> return ()
                putMVar mvar ()
    threadDelay $ 4*seconds

    return (tid,mvar)
-}
