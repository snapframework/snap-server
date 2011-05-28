{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Concurrent (killThread)
import           Control.Concurrent.MVar
import           Control.Monad
import           Prelude hiding (catch)
import qualified Network.HTTP.Enumerator as HTTP
import           Test.Framework (defaultMain, testGroup)
import           System.Environment
import           Snap.Http.Server.Config

import qualified Data.Concurrent.HashMap.Tests
import qualified Snap.Internal.Http.Parser.Tests
import qualified Snap.Internal.Http.Server.Tests
import qualified Snap.Internal.Http.Server.TimeoutManager.Tests
import qualified Test.Blackbox

ports :: Int -> [Int]
ports sp = [sp..]

#ifdef GNUTLS
sslports :: Int -> [Maybe Int]
sslports sp = map Just [(sp + 100)..]
#else
sslports :: Int -> [Maybe Int]
sslports _ = repeat Nothing
#endif

#ifdef LIBEV
backends :: Int -> [(Int,Maybe Int,ConfigBackend)]
backends sp = zip3 (ports sp)
                   (sslports sp)
                   [ConfigSimpleBackend, ConfigLibEvBackend]
#else
backends :: Int -> [(Int,Maybe Int,ConfigBackend)]
backends sp = zip3 (ports sp)
                   (sslports sp)
                   [ConfigSimpleBackend]
#endif

getStartPort :: IO Int
getStartPort = (liftM read (getEnv "STARTPORT") >>= evaluate)
                 `catch` \(_::SomeException) -> return 8111


main :: IO ()
main = HTTP.withHttpEnumerator $ do
    sp <- getStartPort
    let bends = backends sp
    tinfos <- forM bends $ \(port,sslport,b) ->
        Test.Blackbox.startTestServer port sslport b

    defaultMain (tests ++ concatMap blackbox bends) `finally` do
        mapM_ killThread $ map fst tinfos
        mapM_ takeMVar $ map snd tinfos

  where tests =
            [ testGroup "Data.Concurrent.HashMap.Tests"
                        Data.Concurrent.HashMap.Tests.tests
            , testGroup "Snap.Internal.Http.Parser.Tests"
                        Snap.Internal.Http.Parser.Tests.tests
            , testGroup "Snap.Internal.Http.Server.Tests"
                        Snap.Internal.Http.Server.Tests.tests
            , testGroup "Snap.Internal.Http.Server.TimeoutManager.Tests"
                        Snap.Internal.Http.Server.TimeoutManager.Tests.tests
            ]
        blackbox (port, sslport, b) =
            [ testGroup ("Test.Blackbox " ++ backendName)
                        $ Test.Blackbox.tests port backendName
            , testGroup ("Test.Blackbox SSL " ++ backendName)
                        $ Test.Blackbox.ssltests backendName sslport
            ]
          where
            backendName = show b
