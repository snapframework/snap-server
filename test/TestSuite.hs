{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent                             (killThread)
import qualified Control.Exception                              as E
import           Control.Monad                                  (liftM)
import           Data.Maybe                                     (maybeToList)
import           Network                                        (withSocketsDo)
import           System.Environment
import           Test.Framework                                 (defaultMain, testGroup)
------------------------------------------------------------------------------
import qualified Snap.Internal.Http.Server.TLS                  as TLS
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
main = withSocketsDo $ TLS.withTLS $ do
    E.bracket (Test.Blackbox.startTestServers)
              cleanup
              (\tinfos -> do
                  let blackboxTests = bbox tinfos
                  defaultMain $ tests ++ blackboxTests
              )
  where
    cleanup (x, y, m) = mapM_ (killThread . fst) $ [x, y] ++ maybeToList m

    bbox ((_, port), (_, port2), m) =
        [ testGroup "Blackbox" $
          concat [ Test.Blackbox.tests port
                 , Test.Blackbox.haTests port2
                 , Test.Blackbox.ssltests $ fmap snd m
                 ]
        ]

    tests = [ testGroup "Address" Address.tests
            , testGroup "Parser" Parser.tests
            , testGroup "SendFile" SendFile.tests
            , testGroup "Server" Session.tests
            , testGroup "Socket" Socket.tests
            , testGroup "TimeoutManager" TimeoutManager.tests
            ]


------------------------------------------------------------------------------
sslPort :: Int -> Maybe Int

#ifdef OPENSSL
sslPort sp = Just (sp + 100)
#else
sslPort _ = Nothing
#endif

ports :: Int -> (Int, Maybe Int)
ports sp = (sp, sslPort sp)


getStartPort :: IO Int
getStartPort = (liftM read (getEnv "STARTPORT") >>= E.evaluate)
                 `E.catch` \(_::E.SomeException) -> return 8111
