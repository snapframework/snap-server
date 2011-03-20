{-# LANGUAGE CPP #-}

module Main where

import           Control.Exception
import           Control.Concurrent (killThread)
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Network.HTTP.Enumerator as HTTP
import           Test.Framework (defaultMain, testGroup)
import           Snap.Http.Server.Config

import qualified Data.Concurrent.HashMap.Tests
import qualified Snap.Internal.Http.Parser.Tests
import qualified Snap.Internal.Http.Server.Tests
import qualified Snap.Internal.Http.Server.TimeoutManager.Tests
import qualified Test.Blackbox

ports :: [Int]
ports = [8195..]

#ifdef GNUTLS
sslports :: [Maybe Int]
sslports = map Just [8295..]
#else
sslports :: [Maybe Int]
sslports = repeat Nothing
#endif

#ifdef LIBEV
backends :: [(Int,Maybe Int,ConfigBackend)]
backends = zip3 ports sslports [ConfigSimpleBackend, ConfigLibEvBackend]
#else
backends :: [(Int,Maybe Int,ConfigBackend)]
backends = zip3 ports sslports [ConfigSimpleBackend]
#endif

main :: IO ()
main = HTTP.withHttpEnumerator $ do
    tinfos <- forM backends $ \(port,sslport,b) ->
        Test.Blackbox.startTestServer port sslport b

    defaultMain (tests ++ concatMap blackbox backends) `finally` do
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
