{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Blaze.ByteString.Builder          (toByteString)
import           Control.Applicative               ((<$>))
import           Control.Concurrent                (MVar, ThreadId,
                                                    forkIOWithUnmask,
                                                    killThread, newEmptyMVar,
                                                    putMVar, takeMVar)
import           Control.Exception                 (SomeException,
                                                    bracketOnError, evaluate)
import qualified Control.Exception                 as E
import           Control.Monad                     (liftM)
import qualified Data.ByteString.Char8             as S
import qualified Network.Socket                    as N
import           Snap.Core
import           System.Environment                (getArgs)
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Session (httpAcceptLoop,
                                                    snapToServerHandler)
import qualified Snap.Internal.Http.Server.Socket  as Sock
import qualified Snap.Internal.Http.Server.Types   as Types

------------------------------------------------------------------------------
-- | Returns the thread the server is running in as well as the port it is
-- listening on.
startTestSocketServer :: Int -> Snap a -> IO (ThreadId, MVar ())
startTestSocketServer portNum userHandler =
    bracketOnError getSock cleanup forkServer
  where
    getSock = Sock.bindHttp "127.0.0.1" (fromIntegral portNum)

    forkServer sock = do
        port <- fromIntegral <$> N.socketPort sock
        putStrLn $ "starting on " ++ show port
        let scfg = emptyServerConfig { Types._localPort = port }
        mv <- newEmptyMVar
        tid <- forkIOWithUnmask $ \unmask -> do
            putStrLn "server start"
            (unmask $ httpAcceptLoop (snapToServerHandler userHandler)
                                     scfg
                                     (Sock.httpAcceptFunc sock))
                  `E.finally` putMVar mv ()
        return (tid, mv)

    cleanup = N.close

    logAccess _ _               = return ()
    _logError !e                = S.putStrLn $ toByteString e
    onStart _                   = return ()
    onParse _ _                 = return ()
    onUserHandlerFinished _ _ _ = return ()
    onDataFinished _ _ _        = return ()
    onExceptionHook _ _         = return ()
    onEscape _                  = return ()

    emptyServerConfig = Types.ServerConfig logAccess
                                           _logError
                                           onStart
                                           onParse
                                           onUserHandlerFinished
                                           onDataFinished
                                           onExceptionHook
                                           onEscape
                                           "localhost"
                                           0
                                           6
                                           False
                                           1


main :: IO ()
main = do
    portNum <- (((read . head) <$> getArgs) >>= evaluate)
               `E.catch` \(_::SomeException) -> return 3000
    (tid, mv) <- startTestSocketServer portNum $ do
                   modifyResponse $ setContentLength 4
                   writeBS "PONG"
    takeMVar mv
    killThread tid
