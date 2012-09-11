{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Concurrent (killThread)
import           Control.Concurrent.MVar
import           Control.Monad
import           Prelude hiding (catch)
import           Network (withSocketsDo)
import           Test.Framework (defaultMain, testGroup)
import           System.Environment
import           Snap.Http.Server.Config

import qualified Snap.Internal.Http.Parser.Tests
import qualified Snap.Internal.Http.Server.Tests
import qualified Snap.Internal.Http.Server.TimeoutManager.Tests
import qualified Test.Blackbox

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
getStartPort = (liftM read (getEnv "STARTPORT") >>= evaluate)
                 `catch` \(_::SomeException) -> return 8111


main :: IO ()
main = withSocketsDo $ do
    sp <- getStartPort
    let bends = backends sp
    tinfos <- forM bends $ \(port,sslport) ->
        Test.Blackbox.startTestServer port sslport

    defaultMain (tests ++ concatMap blackbox bends) `finally` do
        mapM_ killThread $ map fst tinfos
        mapM_ takeMVar $ map snd tinfos

  where tests =
            [ testGroup "Snap.Internal.Http.Parser.Tests"
                        Snap.Internal.Http.Parser.Tests.tests
            , testGroup "Snap.Internal.Http.Server.Tests"
                        Snap.Internal.Http.Server.Tests.tests
            , testGroup "Snap.Internal.Http.Server.TimeoutManager.Tests"
                        Snap.Internal.Http.Server.TimeoutManager.Tests.tests
            ]
        blackbox (port, sslport) =
            [ testGroup ("Test.Blackbox")
                        $ Test.Blackbox.tests port
            , testGroup ("Test.Blackbox SSL")
                        $ Test.Blackbox.ssltests sslport
            ]
