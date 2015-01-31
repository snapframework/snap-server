{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.Socket.Tests (tests) where

------------------------------------------------------------------------------
import qualified Network.Socket                   as N
------------------------------------------------------------------------------
import           Control.Applicative              ((<$>))
import           Control.Concurrent               (forkIO, killThread, newEmptyMVar, putMVar, readMVar, takeMVar)
import qualified Control.Exception                as E
import           Data.IORef                       (newIORef, readIORef, writeIORef)
import           Test.Framework                   (Test)
import           Test.Framework.Providers.HUnit   (testCase)
import           Test.HUnit                       (assertEqual)
------------------------------------------------------------------------------
import qualified Snap.Internal.Http.Server.Socket as Sock
import           Snap.Test.Common                 (eatException, expectException, withSock)


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testSockClosedOnListenException
        , testAcceptFailure
        ]


------------------------------------------------------------------------------
testSockClosedOnListenException :: Test
testSockClosedOnListenException = testCase "socket/closedOnListenException" $ do
    ref <- newIORef Nothing
    expectException $ Sock.bindSocketImpl (sso ref) bs ls "127.0.0.1" 4444
    (Just sock) <- readIORef ref
    let (N.MkSocket _ _ _ _ mvar) = sock
    readMVar mvar >>= assertEqual "socket closed" N.Closed

  where
    sso ref sock _ _ = do
        let (N.MkSocket _ _ _ _ mvar) = sock
        readMVar mvar >>= assertEqual "socket not connected" N.NotConnected
        writeIORef ref (Just sock) >> fail "set socket option"
    bs _ _ = fail "bindsocket"
    ls _ _ = fail "listen"


------------------------------------------------------------------------------
testAcceptFailure :: Test
testAcceptFailure = testCase "socket/acceptAndInitialize" $ do
    sockmvar <- newEmptyMVar
    donemvar <- newEmptyMVar
    E.bracket (Sock.bindSocket "127.0.0.1" $ fromIntegral N.aNY_PORT)
              (N.close)
              (\s -> do
                   p <- fromIntegral <$> N.socketPort s
                   forkIO $ server s sockmvar donemvar
                   E.bracket (forkIO $ client p)
                             (killThread)
                             (\_ -> do
                                csock <- takeMVar sockmvar
                                takeMVar donemvar
                                N.isConnected csock >>=
                                    assertEqual "closed" False
                             )
              )
  where
    server sock sockmvar donemvar = serve `E.finally` putMVar donemvar ()
      where
        serve = eatException $ E.mask $ \restore ->
                Sock.acceptAndInitialize sock restore $ \(csock, _) -> do
                  putMVar sockmvar csock
                  fail "error"

    client port = withSock port (const $ return ())
