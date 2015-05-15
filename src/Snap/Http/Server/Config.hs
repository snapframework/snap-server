------------------------------------------------------------------------------
-- | This module exports the 'Config' datatype, which you can use to configure
-- the Snap HTTP server.
--
module Snap.Http.Server.Config
  ( Config
  , ConfigLog(..)
  , ProxyType

  , emptyConfig
  , defaultConfig
  , commandLineConfig
  , extendedCommandLineConfig
  , completeConfig

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
  , setUnixSocketAccessMode

  , setStartupHook
  , StartupInfo
  , getStartupSockets
  , getStartupConfig

  -- ** Proxy protocol selection
  , noProxy
  , xForwardedFor
  , haProxy
  ) where

------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Config


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
