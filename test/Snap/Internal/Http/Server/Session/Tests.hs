{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.Session.Tests (tests) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (fromByteString)
import           Blaze.ByteString.Builder.Internal.Buffer (allocBuffer)
import           Control.Concurrent
import           Control.Exception
import           Control.Monad                            (forM_, forever,
                                                           liftM, void, when,
                                                           (>=>))
import           Control.Monad.State.Class                (modify)
import           Data.ByteString.Char8                    (ByteString)
import qualified Data.ByteString.Char8                    as S
import qualified Data.CaseInsensitive                     as CI
import           Data.IORef                               (newIORef)
import qualified Data.Map                                 as Map
import           Data.Maybe                               (isNothing)
import qualified Network.Http.Client                      as Http
import           System.Timeout                           (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import qualified Test.Framework.Runners.Console           as Console
import           Test.HUnit                               hiding (Test, path)
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Internal.Http.Server.Session
import           Snap.Internal.Http.Server.Types
import           Snap.Test                                (RequestBuilder)
import qualified Snap.Test                                as T
import           Snap.Test.Common                         (coverShowInstance, coverTypeableInstance,
                                                           eatException,
                                                           expectException)
import           System.IO.Streams                        (InputStream,
                                                           OutputStream)
import qualified System.IO.Streams                        as Streams
import qualified System.IO.Streams.Concurrent             as Streams
import qualified System.IO.Streams.Debug                  as Streams
------------------------------------------------------------------------------


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testPong
        , testPong1_0
        , testBadParses
        , testEof
        , testHttp100
        , testNoHost
        , testNoHost1_0
        , testChunkedRequest
        , testQueryParams
        , testPostParams
        , testCookie
        , testTrivials
        ]


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

  where
    pong = "PONG"
    snap1 = writeBS pong >> modifyResponse (setContentLength 4)
    snap2 = writeBS pong


------------------------------------------------------------------------------
testPong1_0 :: Test
testPong1_0 = testCase "session/pong1_0" $ do
    is <- makeRequest (T.setHttpVersion (1,0)) >>= Streams.fromList . (:[])
    (os, getInput) <- listOutputStream
    runSession is os (writeBS "PONG")
    out <- liftM S.concat getInput

    assertBool "200 ok" $ S.isPrefixOf "HTTP/1.0 200 OK\r\n" out
    assertBool "PONG" $ S.isSuffixOf "\r\n\r\nPONG" out


    -- this test would be good below, except http-streams has a bug parsing
    -- http/1.1 responses.

{-
    [(resp, body)] <- runRequestPipelineDebug (_makeDebugPipe "pong/1.0")
                                              [T.setHttpVersion (1,0)]
                                              snap
    assertEqual "code" 200 $ Http.getStatusCode resp
    assertEqual "body" pong body
    assertEqual "not chunked" Nothing $ Http.getHeader resp "Transfer-Encoding"
    assertEqual "connection close" (Just "close") $
                Http.getHeader resp "Connection"
  where
    pong = "PONG"
    snap = writeBS pong
-}

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
    check n txt = testCase ("session/badParses/" ++ show n) $ do
        is <- Streams.fromList [txt]
        (os, _) <- listOutputStream
        expectException $ runSession is os (return ())


------------------------------------------------------------------------------
testEof :: Test
testEof = testCase "session/eof" $ do
    l <- runRequestPipeline [] snap
    assertBool "eof1" $ null l

    is <- Streams.fromList [""]
    (os, getList) <- listOutputStream
    runSession is os snap
    out <- getList
    assertEqual "eof2" [] out
  where
    snap = writeBS "OK"


------------------------------------------------------------------------------
testHttp100 :: Test
testHttp100 = testCase "server/expect100" $ do
    is <- makeRequest expect100 >>= Streams.fromList . (:[])
    (os, getList) <- listOutputStream
    runSession is os (writeBS "OK")
    out <- liftM S.concat getList

    assertBool "100-continue" $
               S.isPrefixOf "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK" out

  where
    expect100 = do
        queryGetParams
        T.setHeader "Expect" "100-continue"


------------------------------------------------------------------------------
testNoHost :: Test
testNoHost = testCase "session/noHost" $ do
    is <- Streams.fromList ["GET / HTTP/1.1\r\n\r\n"]
    (os, _) <- listOutputStream
    expectException $ runSession is os (writeBS "OK")


------------------------------------------------------------------------------
testNoHost1_0 :: Test
testNoHost1_0 = testCase "session/noHost1_0" $ do
    is <- Streams.fromList ["GET / HTTP/1.0\r\n\r\n"]
    (os, getList) <- listOutputStream
    runSession is os snap
    out <- liftM S.concat getList
    assertBool "no host 1.0" $ S.isSuffixOf "\r\nbackup-localhost" out
  where
    snap = getRequest >>= writeBS . rqHostName


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
testTrivials :: Test
testTrivials = testCase "session/trivials" $ do
    coverShowInstance $ TerminateSessionException
                      $ SomeException BadRequestException
    coverShowInstance LengthRequiredException
    coverShowInstance BadRequestException
    coverTypeableInstance (undefined :: TerminateSessionException)
    coverTypeableInstance (undefined :: BadRequestException)
    coverTypeableInstance (undefined :: LengthRequiredException)


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
-- | Fill in a 'PerSessionData' with some dummy values.
makePerSessionData :: InputStream ByteString
                   -> OutputStream ByteString
                   -> IO PerSessionData
makePerSessionData readEnd writeEnd = do
    forceConnectionClose <- newIORef False
    let twiddleTimeout f = let z = f 0 in z `seq` return $! ()
    let sendfileHandler _ _ _ _ _ = error "not implemented"
    let localAddress = "127.0.0.1"
    let remoteAddress = "127.0.0.1"
    let remotePort = 43321
    isNewConnection <- newIORef False

    let psd = PerSessionData forceConnectionClose
                             twiddleTimeout
                             sendfileHandler
                             localAddress
                             remoteAddress
                             remotePort
                             readEnd
                             writeEnd
                             isNewConnection

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
runRequestPipelineDebug pipeFunc rbs handler = do
    ((clientRead, clientWrite), (serverRead, serverWrite)) <- pipeFunc

    sigClient <- newEmptyMVar
    results   <- newMVar []

    forM_ rbs $ makeRequest >=> flip Streams.write clientWrite . Just
    Streams.write Nothing clientWrite

    conn <- Http.makeConnection "localhost"
                                (throwIO ThreadKilled)
                                clientWrite
                                clientRead
    m <- mask $ \restore -> do
        ctid <- forkIO $ clientThread restore conn results sigClient
        stid <- forkIO $ restore (runSession serverRead serverWrite handler)
        timeout 10000000 $ restore (takeMVar sigClient) `finally` do
                                        killThread ctid
                                        killThread stid

    when (isNothing m) $ error "timeout"
    readMVar results

  where
    clientThread restore conn results sig =
        eatException (restore loop `finally` putMVar sig ())
      where
        loop = forever $ do
            (resp, body) <- Http.receiveResponse conn $ \rsp istr -> do
                !out <- liftM S.concat $ Streams.toList istr
                return (rsp, out)
            modifyMVar_ results (return . (++ [(resp, body)]))


------------------------------------------------------------------------------
runSession :: InputStream ByteString
           -> OutputStream ByteString
           -> Snap a
           -> IO ()
runSession readEnd writeEnd handler = do
    buffer         <- allocBuffer 64000
    perSessionData <- makePerSessionData readEnd writeEnd
    httpSession buffer (snapToServerHandler handler)
                       (makeServerConfig ())
                       perSessionData
    Streams.write Nothing writeEnd


------------------------------------------------------------------------------
makeServerConfig :: hookState -> ServerConfig hookState
makeServerConfig hs = ServerConfig logAccess
                                   logErr
                                   onStart
                                   onParse
                                   onUserHandlerFinished
                                   onDataFinished
                                   onEx
                                   onEscape
                                   "backup-localhost"
                                   8080
                                   10
                                   False
  where
    logAccess !_ !_                = return $! ()
    logErr !_                      = return $! ()
    onStart !_                     = return hs
    onParse !_ !_                  = return $! ()
    onUserHandlerFinished !_ !_ !_ = return $! ()
    onDataFinished !_ !_ !_        = return $! ()
    onEx !_ !_                     = return $! ()
    onEscape !_                    = return $! ()


------------------------------------------------------------------------------
snapToServerHandler :: Snap a -> ServerHandler hookState
snapToServerHandler snap serverConfig perSessionData req =
    runSnap snap logErr tickle req
  where
    logErr = _logError serverConfig . fromByteString
    tickle = _twiddleTimeout perSessionData


------------------------------------------------------------------------------
listOutputStream :: IO (OutputStream ByteString, IO [ByteString])
listOutputStream = do
    (os, out) <- Streams.listOutputStream
    os' <- Streams.contramapM (evaluate . S.copy) os
    return (os', out)
