{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.Socket.Tests (tests) where

------------------------------------------------------------------------------
import qualified Network.Socket                   as N
------------------------------------------------------------------------------
import           Control.Concurrent               (readMVar)
import           Data.IORef                       (newIORef, readIORef, writeIORef)
import           Test.Framework                   (Test)
import           Test.Framework.Providers.HUnit   (testCase)
import           Test.HUnit                       (assertEqual)
------------------------------------------------------------------------------
import qualified Snap.Internal.Http.Server.Socket as Sock
import           Snap.Test.Common                 (expectException)


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testSockClosedOnListenException
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
