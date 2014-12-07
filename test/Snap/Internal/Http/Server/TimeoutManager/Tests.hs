{-# LANGUAGE OverloadedStrings #-}

module Snap.Internal.Http.Server.TimeoutManager.Tests
  ( tests ) where

------------------------------------------------------------------------------
import           Control.Concurrent                       (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.Thread                (forkIO, result)
import qualified Control.Exception                        as E
import           Control.Monad                            (replicateM)
import           Data.IORef                               (newIORef, readIORef, writeIORef)
import           Data.Maybe                               (isJust)
------------------------------------------------------------------------------
import qualified Snap.Internal.Http.Server.Clock          as Clock
import qualified Snap.Internal.Http.Server.TimeoutManager as TM
import           System.Timeout                           (timeout)
import           Test.Framework                           (Test)
import           Test.Framework.Providers.HUnit           (testCase)
import           Test.HUnit                               (assertBool, assertEqual)

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testOneTimeout
        , testSlowToDie
        , testOneTimeoutAfterInactivity
        , testCancel
        , testTickle ]


------------------------------------------------------------------------------
register :: IO () -> TM.TimeoutManager -> IO TM.TimeoutThread
register m t = TM.register t "test" $
               \restore -> restore (Clock.sleepFor 9000)
                           `E.finally` m


------------------------------------------------------------------------------
testOneTimeout :: Test
testOneTimeout = testCase "timeout/oneTimeout" $ repeatedly $ do
    mgr <- TM.initialize 1 0.1 Clock.getClockTime
    oneTimeout mgr

------------------------------------------------------------------------------
testSlowToDie :: Test
testSlowToDie = testCase "timeout/slowToDie" $ repeatedly $ do
    mgr <- TM.initialize 1 0.1 Clock.getClockTime
    r   <- newIORef False
    s   <- newIORef False
    _   <- register (writeIORef r True >> Clock.sleepFor 3 >> writeIORef s True) mgr
    Clock.sleepFor 1.5
    readIORef r >>= assertEqual "started to die" True
    readIORef s >>= assertEqual "not dead yet" False
    Clock.sleepFor 3
    readIORef s >>= assertEqual "dead" True


------------------------------------------------------------------------------
testOneTimeoutAfterInactivity :: Test
testOneTimeoutAfterInactivity =
    testCase "timeout/oneTimeoutAfterInactivity" $ repeatedly $ do
        mgr <- TM.initialize 1 0.1 Clock.getClockTime
        Clock.sleepFor 3
        oneTimeout mgr

------------------------------------------------------------------------------
repeatedly :: IO () -> IO ()
repeatedly m = dieIfTimeout $ do
    results <- replicateM 40 (forkIO m) >>= sequence . map snd
    mapM_ result results


------------------------------------------------------------------------------
oneTimeout :: TM.TimeoutManager -> IO ()
oneTimeout mgr = do
    mv  <- newEmptyMVar
    _   <- register (putMVar mv ()) mgr
    m   <- timeout (3*seconds) $ takeMVar mv
    assertBool "timeout fired" $ isJust m
    Clock.sleepFor 2
    TM.stop mgr


------------------------------------------------------------------------------
testTickle :: Test
testTickle = testCase "timeout/tickle" $ repeatedly $ do
    mgr <- TM.initialize 5 0.1 Clock.getClockTime
    ref <- newIORef (0 :: Int)
    h <- register (writeIORef ref 1) mgr
    E.evaluate (length $ show h)
    Clock.sleepFor 1
    b0 <- readIORef ref
    assertEqual "b0" 0 b0
    TM.tickle h 3
    Clock.sleepFor 1
    b1 <- readIORef ref
    assertEqual "b1" 0 b1
    Clock.sleepFor 5
    b2 <- readIORef ref
    assertEqual "b2" 1 b2
    TM.stop mgr


------------------------------------------------------------------------------
testCancel :: Test
testCancel = testCase "timeout/cancel" $ repeatedly $ do
    mgr <- TM.initialize 3 0.1 Clock.getClockTime
    ref <- newIORef (0 :: Int)
    h <- register (writeIORef ref 1) mgr
    Clock.sleepFor 1
    readIORef ref >>= assertEqual "b0" 0
    TM.cancel h
    TM.tickle h 10              -- make sure tickle ignores cancelled times
    Clock.sleepFor 2
    readIORef ref >>= assertEqual "b1" 1
    Clock.sleepFor 2
    h' <- register (writeIORef ref 2) mgr
    _ <- register (return ()) mgr
    TM.set h' 1
    Clock.sleepFor 2
    readIORef ref >>= assertEqual "b2" 2
    _   <- register (writeIORef ref 3) mgr
    hs <- replicateM 1000 $! register (return ()) mgr
    mapM TM.cancel hs
    TM.stop mgr
    Clock.sleepFor 1
    readIORef ref >>= assertEqual "b3" 3


------------------------------------------------------------------------------
seconds :: Int
seconds = (10::Int) ^ (6::Int)


------------------------------------------------------------------------------
dieIfTimeout :: IO a -> IO a
dieIfTimeout m = timeout (30 * seconds) m >>= maybe (error "timeout") return
