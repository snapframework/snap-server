{-# LANGUAGE CPP #-}

module Main where

import           Control.Exception
import           Control.Concurrent (killThread)
import           Control.Concurrent.MVar
import           Control.Monad
import           Test.Framework (defaultMain, testGroup)
import           Snap.Http.Server.Config


import qualified Data.Concurrent.HashMap.Tests
import qualified Snap.Internal.Http.Parser.Tests
import qualified Snap.Internal.Http.Server.Tests
import qualified Test.Blackbox

ports :: [Int]
ports = [8195..]

#ifdef GNUTLS
sslports :: [Maybe (Int,Int)]
sslports = map Just $ zip [8295..] [8395..]
#else
sslports :: [Maybe (Int,Int)]
sslports = repeat Nothing
#endif

#ifdef LIBEV
backends :: [(Int,Maybe (Int,Int),ConfigBackend)]
backends = zip3 ports sslports [ConfigSimpleBackend, ConfigLibEvBackend]
#else
backends :: [(Int,Maybe (Int,Int),ConfigBackend)]
backends = zip3 ports sslports [ConfigSimpleBackend]
#endif

main :: IO ()
main = do
    tinfos <- forM backends $ \(port,sslport,b) ->
        Test.Blackbox.startTestServer port sslport b

    stunnels <- forM backends $ \(_,sslport,_) -> do
        Test.Blackbox.spawnStunnel sslport

    defaultMain (tests ++ concatMap blackbox backends) `finally` do
        mapM_ Test.Blackbox.killStunnel stunnels
        mapM_ killThread $ map fst tinfos
        mapM_ takeMVar $ map snd tinfos

  where tests =
            [ testGroup "Data.Concurrent.HashMap.Tests"
                        Data.Concurrent.HashMap.Tests.tests
            , testGroup "Snap.Internal.Http.Parser.Tests"
                        Snap.Internal.Http.Parser.Tests.tests
            , testGroup "Snap.Internal.Http.Server.Tests"
                        Snap.Internal.Http.Server.Tests.tests
            ]
        blackbox (port, sslport, b) =
            [ testGroup ("Test.Blackbox " ++ backendName)
                        $ Test.Blackbox.tests port backendName
            , testGroup ("Test.Blackbox SSL " ++ backendName)
                        $ Test.Blackbox.ssltests backendName sslport
            ]
          where
            backendName = show b
