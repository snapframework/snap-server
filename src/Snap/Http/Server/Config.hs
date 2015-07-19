{-# LANGUAGE OverloadedStrings #-}

-- | Types used by the Snap HTTP Server.
module Snap.Http.Server.Config
  ( ServerConfig
  , PerSessionData

  -- * ServerConfig
  , emptyServerConfig

  -- ** getters\/setters
  , getDefaultTimeout
  , getIsSecure
  , getLocalHostname
  , getLogAccess
  , getLogError
  , getNumAcceptLoops
  , getOnDataFinished
  , getOnEscape
  , getOnException
  , getOnNewRequest
  , getOnParse
  , getOnUserHandlerFinished
  , setDefaultTimeout
  , setIsSecure
  , setLocalHostname
  , setLogAccess
  , setLogError
  , setNumAcceptLoops
  , setOnDataFinished
  , setOnEscape
  , setOnException
  , setOnNewRequest
  , setOnParse
  , setOnUserHandlerFinished

  -- * PerSessionData
  -- ** getters
  , getTwiddleTimeout
  , isNewConnection
  , getLocalAddress
  , getLocalPort
  , getRemoteAddress
  , getRemotePort

  -- * HTTP lifecycle
  -- $lifecycle

  -- * Hooks
  -- $hooks

  , DataFinishedHook
  , EscapeSnapHook
  , ExceptionHook
  , ParseHook
  , NewRequestHook
  , UserHandlerFinishedHook

  -- * Handlers
  , SendFileHandler
  , ServerHandler
  , AcceptFunc

  -- * Socket types
  , SocketConfig(..)
  ) where

------------------------------------------------------------------------------
import           Data.ByteString                 (ByteString)
import           Data.IORef                      (readIORef)
import           Data.Word                       (Word64)
------------------------------------------------------------------------------
import           Data.ByteString.Builder         (Builder)
------------------------------------------------------------------------------
import           Snap.Core                       (Request, Response)
import           Snap.Internal.Http.Server.Types (AcceptFunc, DataFinishedHook, EscapeSnapHook, ExceptionHook, NewRequestHook, ParseHook, PerSessionData (_isNewConnection, _localAddress, _localPort, _remoteAddress, _remotePort, _twiddleTimeout), SendFileHandler, ServerConfig (..), ServerHandler, SocketConfig (..), UserHandlerFinishedHook)


                          ---------------------------
                          -- snap server lifecycle --
                          ---------------------------

------------------------------------------------------------------------------
-- $lifecycle
--
-- 'Request' \/ 'Response' lifecycle for \"normal\" requests (i.e. without
-- errors):
--
-- 1. Accept a new connection, set it up (e.g. with SSL), create a
-- 'PerSessionData' object.
--
-- 2. Enter the 'SessionHandler', which runs the following in a loop:
--   a. The 'NewRequestHook' is called, making a new hookState object to be
--      passed into the rest of the user hooks.
--   b. The HTTP request is parsed. If the session is over (i.e. timeout or
--      connection closed), we stop here and tear down the connection.
--   c. The 'ParseHook' is called.
--   d. We enter 'ServerHandler', which is a wrapper around the user's handler.
--   e. User handler finishes, providing a 'Response'.
--   f. The 'UserHandlerFinishedHook' is called.
--   g. The headers and body of the 'Response' are sent to the client.
--   h. The 'DataFinishedHook' is called.


                                  -----------
                                  -- hooks --
                                  -----------

------------------------------------------------------------------------------
-- $hooks
-- #hooks#
--
-- At various critical points in the HTTP lifecycle, the Snap server will call
-- user-defined \"hooks\" that can be used for instrumentation or tracing of
-- the process of building the HTTP response. The first hook called, the
-- 'NewRequestHook', will generate a \"hookState\" object (having some
-- user-defined abstract type), and this object will be passed to the rest of
-- the hooks as the server handles the process of responding to the HTTP
-- request.
--
-- For example, you could pass a set of hooks to the Snap server that measured
-- timings for each URI handled by the server to produce online statistics and
-- metrics using something like @statsd@ (<https://github.com/etsy/statsd>).


------------------------------------------------------------------------------
emptyServerConfig :: ServerConfig a
emptyServerConfig =
    ServerConfig (\_ _ _ -> return $! ())
                 (\_     -> return $! ())
                 (\_     -> return $ error "undefined hook state")
                 (\_ _   -> return $! ())
                 (\_ _ _ -> return $! ())
                 (\_ _ _ -> return $! ())
                 (\_ _   -> return $! ())
                 (\_     -> return $! ())
                 "localhost"
                 30
                 False
                 1


------------------------------------------------------------------------------
getLogAccess :: ServerConfig hookState -> Request -> Response -> Word64 -> IO ()
getLogAccess sc = _logAccess sc


------------------------------------------------------------------------------
getLogError :: ServerConfig hookState -> Builder -> IO ()
getLogError sc = _logError sc


------------------------------------------------------------------------------
getOnNewRequest :: ServerConfig hookState -> NewRequestHook hookState
getOnNewRequest sc = _onNewRequest sc


------------------------------------------------------------------------------
getOnParse :: ServerConfig hookState -> ParseHook hookState
getOnParse sc = _onParse sc


------------------------------------------------------------------------------
getOnUserHandlerFinished :: ServerConfig hookState
                         -> UserHandlerFinishedHook hookState
getOnUserHandlerFinished sc = _onUserHandlerFinished sc


------------------------------------------------------------------------------
getOnDataFinished :: ServerConfig hookState -> DataFinishedHook hookState
getOnDataFinished sc = _onDataFinished sc


------------------------------------------------------------------------------
getOnException :: ServerConfig hookState -> ExceptionHook hookState
getOnException sc = _onException sc


------------------------------------------------------------------------------
getOnEscape :: ServerConfig hookState -> EscapeSnapHook hookState
getOnEscape sc = _onEscape sc


------------------------------------------------------------------------------
getLocalHostname :: ServerConfig hookState -> ByteString
getLocalHostname sc = _localHostname sc


------------------------------------------------------------------------------
getDefaultTimeout :: ServerConfig hookState -> Int
getDefaultTimeout sc = _defaultTimeout sc


------------------------------------------------------------------------------
getIsSecure :: ServerConfig hookState -> Bool
getIsSecure sc = _isSecure sc


------------------------------------------------------------------------------
getNumAcceptLoops :: ServerConfig hookState -> Int
getNumAcceptLoops sc = _numAcceptLoops sc


------------------------------------------------------------------------------
setLogAccess :: (Request -> Response -> Word64 -> IO ())
             -> ServerConfig hookState
             -> ServerConfig hookState
setLogAccess s sc = sc { _logAccess = s }


------------------------------------------------------------------------------
setLogError :: (Builder -> IO ())
            -> ServerConfig hookState
            -> ServerConfig hookState
setLogError s sc = sc { _logError = s }


------------------------------------------------------------------------------
setOnNewRequest :: NewRequestHook hookState
                -> ServerConfig hookState
                -> ServerConfig hookState
setOnNewRequest s sc = sc { _onNewRequest = s }


------------------------------------------------------------------------------
setOnParse :: ParseHook hookState
           -> ServerConfig hookState
           -> ServerConfig hookState
setOnParse s sc = sc { _onParse = s }


------------------------------------------------------------------------------
setOnUserHandlerFinished :: UserHandlerFinishedHook hookState
                         -> ServerConfig hookState
                         -> ServerConfig hookState
setOnUserHandlerFinished s sc = sc { _onUserHandlerFinished = s }


------------------------------------------------------------------------------
setOnDataFinished :: DataFinishedHook hookState
                  -> ServerConfig hookState
                  -> ServerConfig hookState
setOnDataFinished s sc = sc { _onDataFinished = s }


------------------------------------------------------------------------------
setOnException :: ExceptionHook hookState
               -> ServerConfig hookState
               -> ServerConfig hookState
setOnException s sc = sc { _onException = s }


------------------------------------------------------------------------------
setOnEscape :: EscapeSnapHook hookState
            -> ServerConfig hookState
            -> ServerConfig hookState
setOnEscape s sc = sc { _onEscape = s }


------------------------------------------------------------------------------
setLocalHostname :: ByteString
                 -> ServerConfig hookState
                 -> ServerConfig hookState
setLocalHostname s sc = sc { _localHostname = s }


------------------------------------------------------------------------------
setDefaultTimeout :: Int -> ServerConfig hookState -> ServerConfig hookState
setDefaultTimeout s sc = sc { _defaultTimeout = s }


------------------------------------------------------------------------------
setIsSecure :: Bool -> ServerConfig hookState -> ServerConfig hookState
setIsSecure s sc = sc { _isSecure = s }


------------------------------------------------------------------------------
setNumAcceptLoops :: Int -> ServerConfig hookState -> ServerConfig hookState
setNumAcceptLoops s sc = sc { _numAcceptLoops = s }


------------------------------------------------------------------------------
getTwiddleTimeout :: PerSessionData -> ((Int -> Int) -> IO ())
getTwiddleTimeout psd = _twiddleTimeout psd


------------------------------------------------------------------------------
isNewConnection :: PerSessionData -> IO Bool
isNewConnection = readIORef . _isNewConnection


------------------------------------------------------------------------------
getLocalAddress :: PerSessionData -> ByteString
getLocalAddress psd = _localAddress psd


------------------------------------------------------------------------------
getLocalPort :: PerSessionData -> Int
getLocalPort psd = _localPort psd


------------------------------------------------------------------------------
getRemoteAddress :: PerSessionData -> ByteString
getRemoteAddress psd = _remoteAddress psd


------------------------------------------------------------------------------
getRemotePort :: PerSessionData -> Int
getRemotePort psd = _remotePort psd
