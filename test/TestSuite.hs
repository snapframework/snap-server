{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent                             (killThread)
import qualified Control.Exception                              as E
import           Control.Monad                                  (liftM)
import           Data.Maybe                                     (maybeToList)
import           Network                                        (withSocketsDo)
#ifdef OPENSSL
import           OpenSSL                                        (withOpenSSL)
#endif
import           System.Environment
import           Test.Framework                                 (defaultMain, testGroup)
------------------------------------------------------------------------------
import qualified Snap.Internal.Http.Server.Address.Tests        as Address
import qualified Snap.Internal.Http.Server.Parser.Tests         as Parser
import qualified Snap.Internal.Http.Server.Session.Tests        as Session
import qualified Snap.Internal.Http.Server.Socket.Tests         as Socket
import qualified Snap.Internal.Http.Server.TimeoutManager.Tests as TimeoutManager
import qualified System.SendFile.Tests                          as SendFile
import qualified Test.Blackbox


------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ setupOpenSSL $ do
    let doSSL = False
    E.bracket (Test.Blackbox.startTestServers doSSL)
              cleanup
              (\tinfos -> do
                  let blackboxTests = bbox tinfos
                  defaultMain $ tests ++ blackboxTests
              )
  where
    cleanup (x, y, m) = mapM_ (killThread . fst) $ [x, y] ++ maybeToList m

    bbox ((_, port), (_, port2), m) =
        concat [ Test.Blackbox.tests port
               , Test.Blackbox.haTests port2
               , Test.Blackbox.ssltests $ fmap snd m
               ]

    tests = [ testGroup "Address" Address.tests
            , testGroup "Parser" Parser.tests
            , testGroup "SendFile" SendFile.tests
            , testGroup "Server" Session.tests
            , testGroup "Socket" Socket.tests
            , testGroup "TimeoutManager" TimeoutManager.tests
            ]


------------------------------------------------------------------------------
setupOpenSSL :: IO () -> IO ()
sslPort :: Int -> Maybe Int

#ifdef OPENSSL
setupOpenSSL = withOpenSSL
sslPort sp = Just (sp + 100)
#else
setupOpenSSL = id
sslPort _ = Nothing
#endif

ports :: Int -> (Int, Maybe Int)
ports sp = (sp, sslPort sp)


getStartPort :: IO Int
getStartPort = (liftM read (getEnv "STARTPORT") >>= E.evaluate)
                 `E.catch` \(_::E.SomeException) -> return 8111
