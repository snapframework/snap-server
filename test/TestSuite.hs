{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent                             (killThread, takeMVar)
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
import           Snap.Test.Common                               (eatException)
#ifdef HAS_SENDFILE
import qualified System.SendFile.Tests                          as SendFile
#endif
import qualified Test.Blackbox


------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ TLS.withTLS $ eatException $ do
    E.bracket (Test.Blackbox.startTestServers)
              cleanup
              (\tinfos -> do
                  let blackboxTests = bbox tinfos
                  defaultMain $ tests ++ blackboxTests
              )
  where
    cleanup (x, y, m) = do
        let backends = [x, y] ++ maybeToList m
        mapM_ (killThread . (\(a, _, _) -> a)) backends
        mapM_ (takeMVar . (\(_, _, a) -> a)) backends

    bbox ((_, port, _), (_, port2, _), m) =
        [ testGroup "Blackbox" $
          concat [ Test.Blackbox.tests port
                 , Test.Blackbox.haTests port2
                 , Test.Blackbox.ssltests $ fmap (\(_,x,_) -> x) m
                 ]
        ]

    tests = [ testGroup "Address" Address.tests
            , testGroup "Parser" Parser.tests
#ifdef HAS_SENDFILE
            , testGroup "SendFile" SendFile.tests
#endif
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
