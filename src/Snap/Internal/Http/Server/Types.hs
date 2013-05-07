------------------------------------------------------------------------------
-- | Types internal to the implementation of the Snap HTTP server.
module Snap.Internal.Http.Server.Types
  ( ServerConfig(..)
  , PerSessionData(..)

  -- * HTTP lifecycle
  -- $lifecycle

  -- * Hooks
  -- $hooks
  , DataFinishedHook
  , EscapeSnapHook
  , ExceptionHook
  , ParseHook
  , StartHook
  , UserHandlerFinishedHook

  -- * Handlers
  , SendFileHandler
  , ServerHandler
  , SessionHandler
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (Builder)
import           Blaze.ByteString.Builder.Internal.Buffer (Buffer)
import           Control.Exception                        (SomeException)
import           Data.ByteString                          (ByteString)
import           Data.Int                                 (Int64)
import           Data.IORef                               (IORef)
import           Snap.Core                                (Request, Response)
import           System.IO.Streams                        (InputStream,
                                                           OutputStream)

                          ---------------------------
                          -- snap server lifecycle --
                          ---------------------------

------------------------------------------------------------------------------
-- $lifecycle
--
-- 'Request' \/ 'Response' lifecycle for \"normal\" requests (i.e. without
-- errors):
--
-- 1. accept a new connection, set it up (e.g. with SSL)
--
-- 2. create a 'PerSessionData' object
--
-- 3. Enter the 'SessionHandler', which:
--
-- 4. calls the 'StartHook', making a new hookState object.
--
-- 5. parses the HTTP request. If the session is over, we stop here.
--
-- 6. calls the 'ParseHook'
--
-- 7. enters the 'ServerHandler', which is provided by another part of the
--    framework
--
-- 8. the server handler passes control to the user handler
--
-- 9. a 'Response' is produced, and the 'UserHandlerFinishedHook' is called.
--
-- 10. the 'Response' is written to the client
--
-- 11. the 'DataFinishedHook' is called.
--
-- 12. we go to #3.


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
-- 'StartHook', will generate a \"hookState\" object (having some user-defined
-- abstract type), and this object will be passed to the rest of the hooks as
-- the server handles the process of responding to the HTTP request.
--
-- For example, you could pass a set of hooks to the Snap server that measured
-- timings for each URI handled by the server to produce online statistics and
-- metrics using something like @statsd@ (<https://github.com/etsy/statsd>).


------------------------------------------------------------------------------
-- | The 'StartHook' is called once processing for an HTTP request begins, i.e.
-- after the connection has been accepted and we know that there's data
-- available to read from the socket. It produces a custom \"hookState\" that
-- will be passed to the rest of the hooks.
type StartHook hookState = PerSessionData -> IO hookState

-- | The 'ParseHook' is called after the HTTP Request has been parsed by the
-- server, but before the user handler starts running.
type ParseHook hookState = IORef hookState -> Request -> IO ()

-- | The 'UserHandlerFinishedHook' is called once the user handler has finished
-- running, but before the data for the HTTP response starts being sent to the
-- client.
type UserHandlerFinishedHook hookState =
    IORef hookState -> Request -> Response -> IO ()

-- | The 'DataFinishedHook' is called once the server has finished sending the
-- HTTP response to the client.
type DataFinishedHook hookState =
    IORef hookState -> Request -> Response -> IO ()

-- | The 'ExceptionHook' is called if an exception reaches the toplevel of the
-- server, i.e. if an exception leaks out of the user handler or if an
-- exception is raised during the sending of the HTTP response data.
type ExceptionHook hookState = IORef hookState -> SomeException -> IO ()

-- | The 'EscapeSnapHook' is called if the user handler escapes the HTTP
-- session, e.g. for websockets.
type EscapeSnapHook hookState = IORef hookState -> IO ()


                             ---------------------
                             -- data structures --
                             ---------------------
------------------------------------------------------------------------------
-- | Data and services that all HTTP response handlers share.
--
data ServerConfig hookState = ServerConfig
    { _logAccess             :: !(Request -> Response -> IO ())
    , _logError              :: !(Builder -> IO ())
    , _onStart               :: !(StartHook hookState)
    , _onParse               :: !(ParseHook hookState)
    , _onUserHandlerFinished :: !(UserHandlerFinishedHook hookState)
    , _onDataFinished        :: !(DataFinishedHook hookState)
    , _onException           :: !(ExceptionHook hookState)
    , _onEscape              :: !(EscapeSnapHook hookState)

      -- | will be overridden by a @Host@ header if it appears.
    , _localHostname         :: !ByteString
    , _localPort             :: {-# UNPACK #-} !Int
    , _defaultTimeout        :: {-# UNPACK #-} !Int
    , _isSecure              :: !Bool
    }


------------------------------------------------------------------------------
-- | All of the things a session needs to service a single HTTP request.
data PerSessionData = PerSessionData
    { _forceConnectionClose :: {-# UNPACK #-} !(IORef Bool)
    , _twiddleTimeout       :: !((Int -> Int) -> IO ())
    , _sendfileHandler      :: !SendFileHandler
    , _localAddress         :: !ByteString
    , _remoteAddress        :: !ByteString
    , _remotePort           :: {-# UNPACK #-} !Int
    , _readEnd              :: !(InputStream ByteString)
    , _writeEnd             :: !(OutputStream ByteString)
    , _isNewConnection      :: !(IORef Bool)
    }


                             --------------------
                             -- function types --
                             --------------------
------------------------------------------------------------------------------
-- | This function, provided to the web server internals from the outside, is
-- responsible for producing a 'Response' once the server has parsed the
-- 'Request'.
--
type ServerHandler hookState =
        ServerConfig hookState     -- ^ global server config
     -> PerSessionData             -- ^ per-connection data
     -> Request                    -- ^ HTTP request object
     -> IO (Request, Response)


------------------------------------------------------------------------------
-- | A 'SendFileHandler' is called if the user handler requests that a file be
-- sent using @sendfile()@ on systems that support it (Linux, Mac OSX, and
-- FreeBSD).
type SendFileHandler =
       Buffer                   -- ^ builder buffer
    -> Builder                  -- ^ status line and headers
    -> FilePath                 -- ^ file to send
    -> Int64                    -- ^ start offset
    -> Int64                    -- ^ number of bytes
    -> IO ()



                        ------------------------------
                        -- types for server backend --
                        ------------------------------

------------------------------------------------------------------------------
-- | Initializing a server \"backend\" (i.e. listening on a socket, SSL, etc)
-- will produce a function of the following type.
type SessionHandler hookState = ServerConfig hookState
                             -> ServerHandler hookState
                             -> IO ()
