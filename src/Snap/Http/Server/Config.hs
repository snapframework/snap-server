{-|

This module exports the 'Config' datatype, which you can use to configure the
Snap HTTP server.

-}

module Snap.Http.Server.Config
  ( Config
  , ConfigBackend(..)
  , ConfigLog(..)

  , emptyConfig
  , defaultConfig
  , commandLineConfig
  , extendedCommandLineConfig
  , completeConfig

  , optDescrs
  , fmapOpt

  , getAccessLog
  , getBackend
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
  , getSSLPort
  , getVerbose
  , getStartupHook

  , setAccessLog
  , setBackend
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
  , setSSLPort
  , setVerbose
  , setStartupHook

  , StartupInfo
  , getStartupSockets
  , getStartupConfig
  ) where

import Snap.Internal.Http.Server.Config
