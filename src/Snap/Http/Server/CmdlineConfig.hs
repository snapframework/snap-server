{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

------------------------------------------------------------------------------
-- | /FIXME/ replace this:
--
-- This module exports the 'Config' datatype, which you can use to configure
-- the Snap HTTP server.
--
module Snap.Http.Server.CmdlineConfig
  (
  -- * Types
    CmdlineConfig
  , CmdlineConfigAccessLog(..)
  , CmdlineConfigErrLog(..)
  , ProxyType

  -- * Defaults for CmdlineConfigs
  , emptyCmdlineConfig
  , defaultCmdlineConfig
  , cmdlineConfig
  , extendedCmdlineConfig
  , completeCmdlineConfig

  -- * Evaluating command-line configs
  , toServerConfig


  -- * GetOpt support
  , optDescrs
  , fmapOpt

  -- * Getters and setters for command-line configs
  -- ** Getters
  , getAccessLog
  , getBind
  , getCompression
  , getDefaultTimeout
  , getErrorHandler
  , getErrorLog
  , getHostname
  , getLocale
  , getOther
  , getPort
  , getProxyType
  , getSSLBind
  , getSSLCert
  , getSSLKey
  , getSSLChainCert
  , getSSLPort
  , getVerbose
  , getStartupHook
  , getUnixSocket
  , getUnixSocketAccessMode

  -- ** Setters
  , setAccessLog
  , setBind
  , setCompression
  , setDefaultTimeout
  , setErrorHandler
  , setErrorLog
  , setHostname
  , setLocale
  , setOther
  , setPort
  , setProxyType
  , setSSLBind
  , setSSLCert
  , setSSLKey
  , setSSLChainCert
  , setSSLPort
  , setVerbose
  , setStartupHook
  , setUnixSocket
  , setUnixSocketAccessMode

  -- TODO: startupinfo goes away
  , StartupInfo
  , getStartupSockets
  , getStartupConfig

  -- * Proxy protocol selection
  , noProxy
  , xForwardedFor
  , haProxy

  -- * Utilities
  , combinedAccessLogger
  , nullAccessLogger
  , startAccessLogger
  , startErrorLogger
  , nullAccessLogFunc
  , nullErrorLogFunc
  , defaultErrorLogFunc
  ) where

------------------------------------------------------------------------------
import qualified Control.Exception                       as E
import           Control.Monad                           (mapM, when)
import           Data.ByteString                         (ByteString)
import           Data.ByteString.Builder                 (Builder, byteString, stringUtf8, toLazyByteString)
import qualified Data.ByteString.Builder.Extra           as Builder (flush)
import qualified Data.ByteString.Char8                   as S
import qualified Data.ByteString.Lazy.Char8              as L
import           Data.Maybe                              (catMaybes, fromMaybe)
import           Data.Monoid                             (mappend)
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as T
import           Data.Word                               (Word64)
import           Network                                 (Socket)
import qualified Network.Socket                          as N
import           Prelude                                 (Bool (..), IO, Maybe (..), Show, const, return, show, ($), ($!), (++), (.), (==), (>>=))
import           Snap.Core                               (MonadSnap (..), Request, Response, rqClientAddr, rqHeaders, rqMethod, rqURI, rqVersion, rspStatus)
import           Snap.Internal.Http.Server.Cleanup       (Cleanup)
import qualified Snap.Internal.Http.Server.Cleanup       as Cleanup
import           Snap.Internal.Http.Server.CmdlineConfig
import qualified Snap.Internal.Http.Server.Socket        as Sock
import qualified Snap.Internal.Http.Server.TLS           as TLS
import           Snap.Internal.Http.Server.Types         (AcceptFunc, AccessLogFunc, ErrorLogFunc, ServerConfig (..))
import qualified Snap.Types.Headers                      as H
import qualified System.FastLogger                       as Log
import qualified System.IO.Streams                       as Streams

------------------------------------------------------------------------------
-- | Configure Snap in direct / non-proxying mode.
noProxy :: ProxyType
noProxy = NoProxy


------------------------------------------------------------------------------
-- | Assert that Snap is running behind an HTTP proxy, and that the proxied
-- connection information will be stored in the \"X-Forwarded-For\" or
-- \"Forwarded-For\" HTTP headers.
xForwardedFor :: ProxyType
xForwardedFor = X_Forwarded_For


------------------------------------------------------------------------------
-- | Assert that Snap is running behind a proxy running the HaProxy protocol
-- (see <http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt>).
-- In this mode connections that don't obey the proxy protocol are rejected.
haProxy :: ProxyType
haProxy = HaProxy


------------------------------------------------------------------------------
listeners :: CmdlineConfig m a
          -> [(ByteString, IO (Socket, AcceptFunc), Bool)]
listeners conf = catMaybes [httpListener, httpsListener, unixListener]
  where
    httpsListener = do
        b         <- getSSLBind conf
        p         <- getSSLPort conf
        cert      <- getSSLCert conf
        chainCert <- getSSLChainCert conf
        key       <- getSSLKey conf
        return ( S.concat [ "https://"
                         , b
                         , ":"
                         , bshow p ]
               , do (sock, ctx) <- TLS.bindHttps b p cert chainCert key
                    return (sock, TLS.httpsAcceptFunc sock ctx)
               , True
               )
    httpListener = do
        p <- getPort conf
        b <- getBind conf
        return ( S.concat [ "http://"
                          , b
                          , ":"
                          , bshow p ]
               , do sock <- Sock.bindSocket b p
                    if getProxyType conf == Just HaProxy
                      then return (sock, Sock.haProxyAcceptFunc sock)
                      else return (sock, Sock.httpAcceptFunc sock)
               , False
               )
    unixListener = do
        path <- getUnixSocket conf
        let accessMode = getUnixSocketAccessMode conf
        return ( T.encodeUtf8 . T.pack  $ "unix:" ++ path
               , do sock <- Sock.bindUnixSocket accessMode path
                    return (sock, Sock.httpAcceptFunc sock)
               , False
               )


------------------------------------------------------------------------------
-- | Logs details about a finished request in NCSA "combined" log format to the
-- given log function.
--
combinedAccessLogger :: (Builder -> IO ()) -> AccessLogFunc
combinedAccessLogger logger req rsp cl =
    Log.combinedLogEntry host user reql status cl referer userAgent >>= logger
  where
    hdrs      = rqHeaders req
    host      = rqClientAddr req
    user      = Nothing -- TODO we don't do authentication yet
    (v, v')   = rqVersion req
    ver       = S.concat [ "HTTP/", bshow v, ".", bshow v' ]
    method    = bshow (rqMethod req)
    reql      = S.intercalate " " [ method, rqURI req, ver ]
    status    = rspStatus rsp
    referer   = H.lookup "referer" hdrs
    userAgent = fromMaybe "-" $ H.lookup "user-agent" hdrs


------------------------------------------------------------------------------
nullAccessLogger :: Request -> Response -> Word64 -> IO ()
nullAccessLogger = const $ const $ const $ return $! ()


------------------------------------------------------------------------------
startAccessLogger :: (Builder -> IO ())       -- ^ debug logger for startup
                  -> CmdlineConfigAccessLog
                  -> Cleanup AccessLogFunc
startAccessLogger vlog ConfigNoAccessLog = do
    Cleanup.io $ vlog "access logging disabled"
    return nullAccessLogFunc
startAccessLogger vlog (ConfigIoAccessLog func) = do
    Cleanup.io $ vlog "using custom function for access logging"
    return func
startAccessLogger vlog (ConfigFileAccessLog fp) = do
    Cleanup.io $ vlog ("writing access log to " `mappend` stringUtf8 fp)
    let errFunc s = Streams.write (Just s) Streams.stderr
    logger <- Cleanup.cleanup (Log.newLoggerWithCustomErrorFunction errFunc fp)
                              Log.stopLogger
    return $! combinedAccessLogger (Log.logMsg logger)


------------------------------------------------------------------------------
startErrorLogger ::  (Builder -> IO ())       -- ^ debug logger for startup
                 -> CmdlineConfigErrLog
                 -> Cleanup ErrorLogFunc
startErrorLogger vlog ConfigNoErrLog = do
    Cleanup.io $ vlog "writing access log to stdout"
    return nullErrorLogFunc
startErrorLogger vlog (ConfigIoErrLog func) = do
    Cleanup.io $ vlog "using custom function for error logging"
    return func
startErrorLogger vlog (ConfigFileErrLog fp) = do
    Cleanup.io $ vlog ("writing error log to " `mappend` stringUtf8 fp)
    let errFunc s = Streams.write (Just s) Streams.stderr
    logger <- Cleanup.cleanup (Log.newLoggerWithCustomErrorFunction errFunc fp)
                              Log.stopLogger
    return $! defaultErrorLogFunc (Log.logMsg logger)


------------------------------------------------------------------------------
nullAccessLogFunc :: AccessLogFunc
nullAccessLogFunc _ _ _ = return $! ()


------------------------------------------------------------------------------
nullErrorLogFunc :: ErrorLogFunc
nullErrorLogFunc _ = return $! ()


------------------------------------------------------------------------------
defaultErrorLogFunc :: (Builder -> IO ()) -> ErrorLogFunc
defaultErrorLogFunc logMsg s = do
    s' <- Log.timestampedLogEntry s
    logMsg (s' `mappend` Builder.flush)


------------------------------------------------------------------------------
verboseLog :: Builder -> IO ()
verboseLog s = do
    s' <- Log.timestampedLogEntry s
    let x = S.concat $ L.toChunks $ toLazyByteString s'
    Streams.write (Just x) Streams.stderr


------------------------------------------------------------------------------
toServerConfig :: MonadSnap m
               => ServerConfig s       -- ^ default server config
               -> CmdlineConfig m a    -- ^ command line configuration
               -> Cleanup [(ByteString, ServerConfig s, AcceptFunc)]
               -- ^ bind address, server config, accept function
toServerConfig template cmdline0 = do
    cmdline <- Cleanup.io $ completeCmdlineConfig cmdline0
    let vlog = when (fromMaybe False (getVerbose cmdline)) . verboseLog

    logAccess <- startAccessLogger vlog
                     $ fromMaybe (ConfigFileAccessLog "-")
                     $ getAccessLog cmdline
    logErr <- startErrorLogger vlog
                  $ fromMaybe (ConfigFileErrLog "-")
                  $ getErrorLog cmdline

    let lhost = fromMaybe (_localHostname template) (getHostname cmdline)
    let tout = fromMaybe (_defaultTimeout template) (getDefaultTimeout cmdline)
    let scfg = template { _logAccess      = logAccess
                        , _logError       = logErr
                        , _localHostname  = lhost
                        , _defaultTimeout = tout
                        }
    mapM (startListener vlog scfg) $ listeners cmdline

  where
    startListener vlog scfg0 (name, start, secure) = do
        Cleanup.io $ vlog $ "listening on " `mappend` byteString name
        let scfg = scfg0 { _isSecure = secure }
        let abort = \(sock, _) -> N.close sock
        (_, acceptFunc) <- Cleanup.cleanup start abort
        return $! (name, scfg, acceptFunc)


------------------------------------------------------------------------------
bshow :: (Show a) => a -> ByteString
bshow = S.pack . show
