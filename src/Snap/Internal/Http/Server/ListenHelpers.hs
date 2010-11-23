{-# LANGUAGE CPP #-}

module Snap.Internal.Http.Server.ListenHelpers where

import           Data.ByteString (ByteString)
import           Foreign.C
import           Network.Socket (Socket, sClose)
import           Snap.Internal.Http.Server.Backend
import qualified Snap.Internal.Http.Server.HttpPort as Http
import qualified Snap.Internal.Http.Server.GnuTLS   as TLS

listenSocket :: ListenSocket -> Socket
listenSocket (ListenHttp s) = s
listenSocket (ListenHttps s _ _) = s

isSecure :: ListenSocket -> Bool
isSecure (ListenHttp _)      = False
isSecure (ListenHttps _ _ _) = True

closeSocket :: ListenSocket -> IO ()
closeSocket (ListenHttp s)      = sClose s
closeSocket p@(ListenHttps s _ _) = do TLS.freePort p
                                       sClose s

createSession :: ListenSocket -> Int -> CInt -> IO () -> IO NetworkSession
createSession (ListenHttp _)        = Http.createSession
createSession p@(ListenHttps _ _ _) = TLS.createSession p

endSession :: ListenSocket -> NetworkSession -> IO ()
endSession (ListenHttp _)      = Http.endSession
endSession (ListenHttps _ _ _) = TLS.endSession

#ifdef PORTABLE
-- For portable builds, we can't call read/write directly so we need the original
-- haskell socket to use with network-bytestring package.
-- Only the simple backend creates sockets in haskell so the following
-- functions only work with the simple backend.

recv :: ListenSocket -> Socket -> IO () -> NetworkSession -> IO (Maybe ByteString)
recv (ListenHttp _)      s = Http.recv s
recv (ListenHttps _ _ _) _ = TLS.recv

send :: ListenSocket -> Socket -> IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send (ListenHttp _)      s = Http.send s
send (ListenHttps _ _ _) _ = TLS.send

#else

recv :: ListenSocket -> IO () -> NetworkSession -> IO (Maybe ByteString)
recv (ListenHttp _)      = Http.recv
recv (ListenHttps _ _ _) = TLS.recv

send :: ListenSocket -> IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send (ListenHttp _)      = Http.send
send (ListenHttps _ _ _) = TLS.send

#endif
