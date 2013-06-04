{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception                              as E
import           Control.Monad                                  (forM, liftM)
import           Network                                        (withSocketsDo)
import           System.Environment
import           Test.Framework                                 (defaultMain,
                                                                 testGroup)
------------------------------------------------------------------------------
import qualified Snap.Internal.Http.Server.Parser.Tests         as Parser
import qualified Snap.Internal.Http.Server.Session.Tests        as Session
import qualified Snap.Internal.Http.Server.TimeoutManager.Tests as TimeoutManager
import qualified Test.Blackbox


------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ do
    sp <- getStartPort
    let bends = backends sp
    tinfos <- forM bends $ \(port, sslport) ->
              Test.Blackbox.startTestServer port sslport
    defaultMain tests
  where
    tests = [ testGroup "Parser" Parser.tests
            , testGroup "Server" Session.tests
            , testGroup "TimeoutManager" TimeoutManager.tests
            ]


------------------------------------------------------------------------------
ports :: Int -> [Int]
ports sp = [sp]

#ifdef OPENSSL
sslports :: Int -> [Maybe Int]
sslports sp = map Just [(sp + 100)]
#else
sslports :: Int -> [Maybe Int]
sslports _ = repeat Nothing
#endif

backends :: Int -> [(Int,Maybe Int)]
backends sp = zip (ports sp)
                  (sslports sp)

getStartPort :: IO Int
getStartPort = (liftM read (getEnv "STARTPORT") >>= E.evaluate)
                 `E.catch` \(_::E.SomeException) -> return 8111
