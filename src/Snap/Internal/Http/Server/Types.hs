------------------------------------------------------------------------------
module Snap.Internal.Http.Server.Types
  ( ServerConfig(..)
  , PerSessionData(..)
  , AcceptHook
  , ParseHook
  , DataFinishedHook
  , UserHandlerFinishedHook
  , SessionFinishedHook
  , ServerHandler
  , SessionHandler
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder (Builder)
import           Data.ByteString          (ByteString)
import           Data.Int                 (Int64)
import           Data.IORef               (IORef)
import           Snap.Core                (Request, Response)
import           System.IO.Streams        (InputStream, OutputStream)

                                  -----------
                                  -- hooks --
                                  -----------
------------------------------------------------------------------------------
type AcceptHook hookState = PerSessionData -> IO hookState

type ParseHook hookState = hookState -> Request -> IO hookState

type UserHandlerFinishedHook hookState =
         hookState -> Request -> Response -> IO hookState

type DataFinishedHook hookState = hookState -> Request -> Response -> IO ()

type SessionFinishedHook hookState = hookState -> IO ()

                          ---------------------------
                          -- snap server lifecycle --
                          ---------------------------

-- $lifecycle
--
-- 'Request' \/ 'Response' lifecycle for \"normal\" requests (i.e. without
-- errors):
--
-- 1. accept a new connection, set it up (e.g. with SSL)
--
-- 2. create a 'PerSessionData' object
--
-- 3. call the 'AcceptHook'.
--
-- 4. Enter the 'SessionHandler', which:
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
-- 9. a 'Response' is produced
--
-- 10. the 'Response' is written to the client, we go to #3.
--
--    (NOTE(greg): to get the semantics we want here, we need to call
--    Streams.peek before we call the AcceptHook so that we ensure there's a
--    request coming and our stats don't get distorted by keepalive.


                             ---------------------
                             -- data structures --
                             ---------------------
------------------------------------------------------------------------------
-- | Data and services that all HTTP response handlers share.
--
data ServerConfig hookState = ServerConfig
    { _logAccess             :: !(Request -> Response -> IO ())
    , _logError              :: !(Builder -> IO ())
    , _onAccept              :: !(AcceptHook hookState)
    , _onParse               :: !(ParseHook hookState)
    , _onUserHandlerFinished :: !(UserHandlerFinishedHook hookState)
    , _onDataFinished        :: !(DataFinishedHook hookState)
    , _onSessionFinished     :: !(SessionFinishedHook hookState)

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
    , _sendfileHandler      :: !(FilePath -> Int64 -> Int64 -> IO ())
    , _localAddress         :: !ByteString
    , _remoteAddress        :: !ByteString
    , _remotePort           :: {-# UNPACK #-} !Int
    , _readEnd              :: !(InputStream ByteString)
    , _writeEnd             :: !(OutputStream Builder)
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
-- | Each backend will accept and set up (e.g. with SSL) a new incoming
-- connection, populate a 'PerSessionData' object, and call the provided
-- 'SessionHandler'.
--
type SessionHandler hookState = ServerConfig hookState
                             -> PerSessionData
                             -> IO ()
