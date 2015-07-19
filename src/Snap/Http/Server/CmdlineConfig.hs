{-# LANGUAGE Rank2Types #-}

------------------------------------------------------------------------------
-- | This module exports the 'Config' datatype, which you can use to configure
-- the Snap HTTP server.
--
module Snap.Http.Server.CmdlineConfig
  ( CmdlineConfig
  , CmdlineConfigLog(..)
  , ProxyType

  , emptyCmdlineConfig
  , defaultCmdlineConfig
  , cmdlineConfig
  , extendedCmdlineConfig
  , completeCmdlineConfig

  , generateServerConfig

  , optDescrs
  , fmapOpt

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

  -- ** Proxy protocol selection
  , noProxy
  , xForwardedFor
  , haProxy

  -- ** Utilities
  , combinedAccessLogger
  , nullAccessLogger
  ) where

------------------------------------------------------------------------------
import qualified Control.Exception                       as E
import           Data.IORef                              (IORef)
import qualified Data.IORef                              as IORef
import           Snap.Internal.Http.Server.Cleanup       (Cleanup, WithCleanup, runCleanup)
import qualified Snap.Internal.Http.Server.Cleanup       as Cleanup
import           Snap.Internal.Http.Server.CmdlineConfig


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
listeners :: CmdlineConfig m a -> IO [(ByteString, Socket, AcceptFunc, CmdlineConfig m a)]
listeners conf = TLS.withTLS $ do
  let fs = catMaybes [httpListener, httpsListener, unixListener]
  mapM (\(str, mkAfunc, cfg) -> do (sock, afunc) <- mkAfunc
                                   return $! (str, sock, afunc, cfg)) fs
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
               , conf { isSecure = True }
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
               , conf
               )
    unixListener = do
        path <- getUnixSocket conf
        let accessMode = getUnixSocketAccessMode conf
        return ( T.encodeUtf8 . T.pack  $ "unix:" ++ path
               , do sock <- Sock.bindUnixSocket accessMode path
                    return (sock, Sock.httpAcceptFunc sock)
               , conf
               )


------------------------------------------------------------------------------
-- | Logs details about a finished request in NCSA "combined" log format to the
-- given log function.
--
combinedAccessLogger
  :: (Builder -> IO ())
  -> Request
  -> Response
  -> Word64
  -> IO ()
combinedAccessLogger logger req rsp cl =
    combinedLogEntry host user reql status cl referer userAgent >>= logger
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
toServerConfig :: MonadSnap m
               => CmdlineConfig m a
               -> IO [(ServerConfig s, AcceptFunc, IO ())]
               -- ^ server config, accept function, cleanup action
toServerConfig cmdline0 = do
    let output = when (fromJust $ getVerbose cmdline) . hPutStrLn stderr
    cmdline <- completeCmdlineConfig cmdline0

    let logAccess = getAccessLog
    undefined
