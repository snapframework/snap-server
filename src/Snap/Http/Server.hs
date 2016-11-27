{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | The Snap HTTP server is a high performance, epoll-enabled, iteratee-based
-- web server library written in Haskell. Together with the @snap-core@
-- library upon which it depends, it provides a clean and efficient Haskell
-- programming interface to the HTTP protocol.
--
module Snap.Http.Server
  ( simpleHttpServe
  , httpServe
  , quickHttpServe
  , snapServerVersion
  , setUnicodeLocale
  , module Snap.Http.Server.Config
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent (newMVar, withMVar)
import           Control.Monad
import           Control.Monad.CatchIO
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List
import           Data.Maybe
#if !MIN_VERSION_base(4,6,0)
import           Prelude hiding (catch)
#endif
import           Snap.Http.Server.Config
import qualified Snap.Internal.Http.Server as Int
import           Snap.Internal.Http.Server.Config (emptyStartupInfo,
                                                   setStartupSockets,
                                                   setStartupConfig)
import           Snap.Core
import           Snap.Util.GZip
import           Snap.Util.Proxy
#ifndef PORTABLE
import           System.Posix.Env
#endif
import           System.IO
import           System.FastLogger


------------------------------------------------------------------------------
-- | A short string describing the Snap server version
snapServerVersion :: ByteString
snapServerVersion = Int.snapServerVersion


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler. This function never
-- returns; to shut down the HTTP server, kill the controlling thread.
--
-- This function is like 'httpServe' except it doesn't setup compression,
-- reverse proxy address translation (via 'Snap.Util.Proxy.behindProxy'), or
-- the error handler; this allows it to be used from 'MonadSnap'.
simpleHttpServe :: MonadSnap m => Config m a -> Snap () -> IO ()
simpleHttpServe config handler = do
    conf <- completeConfig config
    let output   = when (fromJust $ getVerbose conf) . hPutStrLn stderr
    mapM_ (output . ("Listening on "++) . show) $ listeners conf

    go conf `finally` output "\nShutting down..."

  where
    --------------------------------------------------------------------------
    go conf = do
        let tout = fromMaybe 60 $ getDefaultTimeout conf

        setUnicodeLocale $ fromJust $ getLocale conf
        withLoggers (fromJust $ getAccessLog conf)
                    (fromJust $ getErrorLog conf) $ \(alog, elog) ->
                        Int.httpServe tout
                          (listeners conf)
                          (fromJust $ getHostname  conf)
                          alog
                          (getAccessLogHandler conf)
                          elog
                          (getErrorLogHandler conf)
                          (\sockets -> let dat = mkStartupInfo sockets conf
                                       in maybe (return ())
                                                ($ dat)
                                                (getStartupHook conf))
                          (runSnap handler)

    --------------------------------------------------------------------------
    mkStartupInfo sockets conf =
        setStartupSockets sockets $
        setStartupConfig conf emptyStartupInfo

    --------------------------------------------------------------------------
    maybeSpawnLogger f (ConfigFileLog fp) =
        liftM Just $ newLoggerWithCustomErrorFunction f fp
    maybeSpawnLogger _ _                  = return Nothing

    --------------------------------------------------------------------------
    maybeIoLog (ConfigIoLog a) = Just a
    maybeIoLog _               = Nothing

    --------------------------------------------------------------------------
    withLoggers afp efp act =
        bracket (do mvar <- newMVar ()
                    let f s = withMVar mvar
                                (const $ BS.hPutStr stderr s >> hFlush stderr)
                    alog <- maybeSpawnLogger f afp
                    elog <- maybeSpawnLogger f efp
                    return (alog, elog))
                (\(alog, elog) -> do
                    maybe (return ()) stopLogger alog
                    maybe (return ()) stopLogger elog)
                (\(alog, elog) -> act ( liftM logMsg alog <|> maybeIoLog afp
                                      , liftM logMsg elog <|> maybeIoLog efp))
{-# INLINE simpleHttpServe #-}


------------------------------------------------------------------------------
listeners :: Config m a -> [Int.ListenPort]
listeners conf = catMaybes [ httpListener, httpsListener ]
  where
    httpsListener = do
        b         <- getSSLBind conf
        p         <- getSSLPort conf
        cert      <- getSSLCert conf
        chainCert <- getSSLChainCert conf
        key       <- getSSLKey conf
        return $! Int.HttpsPort b p cert chainCert key

    httpListener = do
        p <- getPort conf
        b <- getBind conf
        return $! Int.HttpPort b p


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler, with settings from
-- the 'Config' passed in. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
httpServe :: Config Snap a -> Snap () -> IO ()
httpServe config handler0 = do
    conf <- completeConfig config
    let !handler = chooseProxy conf
    let serve = compress conf . catch500 conf $ handler
    simpleHttpServe conf serve

  where
    chooseProxy conf = maybe handler0
                             (\ptype -> behindProxy ptype handler0)
                             (getProxyType conf)
{-# INLINE httpServe #-}


------------------------------------------------------------------------------
catch500 :: MonadSnap m => Config m a -> m () -> m ()
catch500 conf = flip catch $ fromJust $ getErrorHandler conf
{-# INLINE catch500 #-}


------------------------------------------------------------------------------
compress :: MonadSnap m => Config m a -> m () -> m ()
compress conf = if fromJust $ getCompression conf then withCompression else id
{-# INLINE compress #-}


------------------------------------------------------------------------------
-- | Starts serving HTTP using the given handler. The configuration is read
-- from the options given on the command-line, as returned by
-- 'commandLineConfig'. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
quickHttpServe :: Snap () -> IO ()
quickHttpServe m = commandLineConfig emptyConfig >>= \c -> httpServe c m


------------------------------------------------------------------------------
-- | Given a string like \"en_US\", this sets the locale to \"en_US.UTF-8\".
-- This doesn't work on Windows.
setUnicodeLocale :: String -> IO ()
setUnicodeLocale =
#ifndef PORTABLE
    \lang -> mapM_ (\k -> setEnv k (lang ++ ".UTF-8") True)
               [ "LANG"
               , "LC_CTYPE"
               , "LC_NUMERIC"
               , "LC_TIME"
               , "LC_COLLATE"
               , "LC_MONETARY"
               , "LC_MESSAGES"
               , "LC_PAPER"
               , "LC_NAME"
               , "LC_ADDRESS"
               , "LC_TELEPHONE"
               , "LC_MEASUREMENT"
               , "LC_IDENTIFICATION"
               , "LC_ALL" ]
#else
    const $ return ()
#endif
