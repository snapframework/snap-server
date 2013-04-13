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
import           Data.ByteString.Char8                    (ByteString)
import qualified Data.ByteString.Char8                    as S
import           Data.IORef                               (newIORef)
import           Data.Maybe                               (isNothing)
import qualified Network.Http.Client                      as Http
import           System.Timeout                           (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                               hiding (Test, path)
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Internal.Http.Server.Session
import           Snap.Internal.Http.Server.Types
import qualified Snap.Test                                as T
import           Snap.Test.Common                         (eatException)
import           System.IO.Streams                        (InputStream,
                                                           OutputStream)
import qualified System.IO.Streams                        as Streams
import qualified System.IO.Streams.Concurrent             as Streams
------------------------------------------------------------------------------


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testBasicNop ]


------------------------------------------------------------------------------
mkRequest :: T.RequestBuilder IO a -> IO ByteString
mkRequest = (T.buildRequest . void) >=> T.requestToString


------------------------------------------------------------------------------
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
makePipe :: IO ( (InputStream ByteString, OutputStream ByteString)
               , (InputStream ByteString, OutputStream ByteString)
               )
makePipe = do
    chan1 <- newChan
    chan2 <- newChan

    clientReadEnd  <- Streams.chanToInput  chan1
    clientWriteEnd <- Streams.chanToOutput chan2
    serverReadEnd  <- Streams.chanToInput  chan2
    serverWriteEnd <- Streams.chanToOutput chan1

    return ((clientReadEnd, clientWriteEnd), (serverReadEnd, serverWriteEnd))


------------------------------------------------------------------------------
runRequests :: [T.RequestBuilder IO ()]
            -> Snap b
            -> IO [(Http.Response, ByteString)]
runRequests rbs handler = do
    ((clientRead, clientWrite), (serverRead, serverWrite)) <- makePipe

    sigClient <- newEmptyMVar
    results   <- newMVar []

    forM_ rbs (mkRequest >=> flip Streams.write clientWrite . Just)
    Streams.write Nothing clientWrite

    conn <- Http.makeConnection "localhost"
                                (throwIO ThreadKilled)
                                clientWrite
                                clientRead
    m <- mask $ \restore -> do
        ctid <- forkIO $ clientThread restore conn results sigClient
        stid <- forkIO $ restore (serverThread serverRead serverWrite)
        timeout 10000000 $ restore (takeMVar sigClient `finally` do
                                        killThread ctid
                                        killThread stid)

    when (isNothing m) $ error "timeout"
    readMVar results

  where
    serverThread readEnd writeEnd = do
        buffer         <- allocBuffer 64000
        perSessionData <- makePerSessionData readEnd writeEnd
        httpSession buffer (snapToServerHandler handler)
                           (makeServerConfig ())
                           perSessionData
        Streams.write Nothing writeEnd

    clientThread restore conn results sig =
        eatException (restore loop `finally` putMVar sig ())
      where
        loop = forever $ do
            (resp, body) <- Http.receiveResponse conn $ \rsp istr -> do
                !out <- liftM S.concat $ Streams.toList istr
                return (rsp, out)
            modifyMVar_ results (return . (++ [(resp, body)]))


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
                                   "localhost"
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
testBasicNop :: Test
testBasicNop = testCase "session/basicNop" $ do
    [(resp, body)] <- runRequests [return ()] snap
    assertEqual "code" 200 $ Http.getStatusCode resp
    assertEqual "body" "OK" body
  where
    snap = writeBS "OK"

