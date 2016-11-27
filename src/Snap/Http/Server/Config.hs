{-|

This module exports the 'Config' datatype, which you can use to configure the
Snap HTTP server.

-}

module Snap.Http.Server.Config
  ( Config
  , ConfigLog(..)

  , AccessLogHandler
  , ErrorLogHandler

  , emptyConfig
  , defaultConfig
  , commandLineConfig
  , extendedCommandLineConfig
  , completeConfig

  , optDescrs
  , fmapOpt

  , getAccessLog
  , getAccessLogHandler
  , getBind
  , getCompression
  , getDefaultTimeout
  , getErrorHandler
  , getErrorLog
  , getErrorLogHandler
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

  , setAccessLog
  , setAccessLogHandler
  , setBind
  , setCompression
  , setDefaultTimeout
  , setErrorHandler
  , setErrorLog
  , setErrorLogHandler
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
  , StartupInfo
  , getStartupSockets
  , getStartupConfig
  ) where

import Snap.Internal.Http.Server.Config
import Snap.Internal.Http.Server
