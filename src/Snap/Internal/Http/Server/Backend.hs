{-# LANGUAGE CPP #-}
module Snap.Internal.Http.Server.Backend where

{-

The server backend is made up of two APIs.

+ The ListenSocket class abstracts the reading and writing from the network.
  We have seperate implementations of ListenSocket for http and https.

+ The EventLoop function is the interface to accept on the socket.
  The EventLoop function will listen on the ports, and for each accepted
  connection it wil call the SessionHandler.

-}

#ifdef OPENSSL
import OpenSSL.Session
#endif

import GHC.Exts (Any)
import Data.ByteString (ByteString)
import Foreign
import Foreign.C
import Network.Socket (Socket)
import Snap.Iteratee (Iteratee, Enumerator)


------------------------------------------------------------------------------
data SessionInfo = SessionInfo
    { localAddress  :: ByteString
    , localPort     :: Int
    , remoteAddress :: ByteString
    , remotePort    :: Int
    , isSecure      :: Bool
    }


------------------------------------------------------------------------------
type SessionHandler =
       SessionInfo                           -- ^ session port information
    -> Enumerator ByteString IO ()           -- ^ read end of socket
    -> Iteratee ByteString IO ()             -- ^ write end of socket
    -> (FilePath -> Int64 -> Int64 -> IO ()) -- ^ sendfile end
    -> ((Int -> Int) -> IO ())               -- ^ timeout tickler
    -> IO ()


------------------------------------------------------------------------------
type EventLoop = Int                       -- ^ default timeout
              -> [ListenSocket]            -- ^ list of ports
              -> Int                       -- ^ number of capabilities
              -> (ByteString -> IO ())     -- ^ error log
              -> IO ()                     -- ^ initialisation function
              -> SessionHandler            -- ^ session handler
              -> IO ()


{- For performance reasons, we do not implement this as a class
class ListenSocket a where
    data ListenSocketSession a :: *

    listenSocket  :: a -> Socket
    isSecure      :: a -> Bool

    closePort     :: a -> IO ()

    createSession :: a
                  -> Int   -- ^ recv buffer size
                  -> CInt  -- ^ network socket
                  -> IO () -- ^ action to block waiting for handshake
                  -> IO (ListenSocketSession a)

    endSession    :: a -> ListenSocketSession a -> IO ()

    recv :: a
         -> IO ()                 -- ^ action to block waiting for data
         -> ListenSocketSession a  -- ^ session
         -> IO (Maybe ByteString)

    send :: a
         -> IO ()                 -- ^ action to tickle the timeout
         -> IO ()                 -- ^ action to block waiting for data
         -> ListenSocketSession a  -- ^ session
         -> ByteString            -- ^ data to send
         -> IO ()
-}


------------------------------------------------------------------------------
data ListenSocket = ListenHttp  Socket
#ifdef OPENSSL
                  | ListenHttps Socket SSLContext
#else
                  | ListenHttps Socket ()
#endif
instance Show ListenSocket where
    show (ListenHttp s)    = "ListenHttp ("  ++ show s ++ ")"
    show (ListenHttps s _) = "ListenHttps (" ++ show s ++ ")"


------------------------------------------------------------------------------
data NetworkSession = NetworkSession
  { _socket     :: CInt
  , _session    :: Any          -- ^ brutal hack.
  , _recvLen    :: Int
  }
