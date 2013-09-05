{-|

This module exports the 'Config' datatype, which you can use to configure the
Snap HTTP server.

-}

module Snap.Http.Server.Config
  ( Config
  , ConfigLog(..)

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
  , getSSLPort
  , getVerbose
  , getStartupHook
  , getMaxPOSTBodySize

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
  , setSSLPort
  , setVerbose
  , setStartupHook
  , setMaxPOSTBodySize
  , StartupInfo
  , getStartupSockets
  , getStartupConfig
  ) where

import Snap.Internal.Http.Server.Config
