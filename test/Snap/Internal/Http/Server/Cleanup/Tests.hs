module Snap.Internal.Http.Server.Cleanup.Tests (tests) where

import           Control.Applicative               (Applicative (..), (<$), (<$>))
import qualified Control.Exception                 as E
import           Data.IORef                        (modifyIORef, newIORef, readIORef, writeIORef)
import           Snap.Internal.Http.Server.Cleanup (Cleanup)
import qualified Snap.Internal.Http.Server.Cleanup as Cleanup
import           Test.Framework                    (Test)
import           Test.Framework.Providers.HUnit    (testCase)
import           Test.HUnit                        (assertEqual)

import           Snap.Test.Common                  (eatException, expectException)

tests :: [Test]
tests = [ testBasic, testTrivials, testAllocException ]

testBasic :: Test
testBasic = testCase "cleanup/basic" $ do
    ref <- newIORef (0 :: Int)
    Cleanup.runCleanup $ cleanup ref
    readIORef ref >>= assertEqual "cleanup happened" 1

  where
    cleanup ref = do
        Cleanup.io (readIORef ref >>= assertEqual "ref is 0" 0)
        ref2 <- Cleanup.cleanup (return ref) (flip writeIORef 1)
        Cleanup.io (readIORef ref2 >>=
                    assertEqual "ref is 0 after Cleanup.cleanup" 0)

testAllocException :: Test
testAllocException = testCase "cleanup/allocException" $ do
    ref <- newIORef ([] :: [Int])
    eatException $ Cleanup.runCleanup $ do
        Cleanup.cleanup (return ref) (flip modifyIORef (1:))
        Cleanup.cleanup (return ref) (flip modifyIORef (2:))
        Cleanup.cleanup (fail "error") undefined
    readIORef ref >>= assertEqual "cleanups should happen LIFO" [1,2]

testTrivials :: Test
testTrivials = testCase "cleanup/trivials" $ do
    Cleanup.runCleanup ((0*) <$> cio)
    expectException (Cleanup.runCleanup ctot)
    Cleanup.runCleanup (() <$ cappl)

   where
    cio :: Cleanup Int
    cio = return 0

    ctot :: Cleanup ()
    ctot = cio >>= \x -> cio >> fail (show x)

    cappl :: Cleanup Int
    cappl = cio <* (pure const <*> cio) *> cio
