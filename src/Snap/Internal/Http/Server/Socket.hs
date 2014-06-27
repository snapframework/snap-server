{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snap.Internal.Http.Server.Socket
  ( bindSocket
  , bindSocketImpl
  , httpAcceptFunc
  , haProxyAcceptFunc
  , sendFileFunc
  ) where

------------------------------------------------------------------------------
import           Control.Exception                 (bracketOnError)
import           Data.ByteString.Char8             (ByteString)
import           Network.Socket                    (Socket, SocketOption (NoDelay, ReuseAddr), accept, close, getSocketName, listen, setSocketOption, socket)
import qualified Network.Socket                    as N
#ifdef HAS_SENDFILE
import           Control.Exception                 (bracket)
import           Network.Socket                    (fdSocket)
import           System.Posix.IO                   (OpenMode (..), closeFd, defaultFileFlags, openFd)
import           System.Posix.Types                (Fd (..))
import           System.SendFile                   (sendFile, sendHeaders)
#else
import           Blaze.ByteString.Builder          (fromByteString)
import           Network.Socket.ByteString         (sendAll)
#endif
------------------------------------------------------------------------------
import qualified System.IO.Streams                 as Streams
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Address (getAddress, getSockAddr)
import           Snap.Internal.Http.Server.Types   (AcceptFunc (..), SendFileHandler)
import qualified System.IO.Streams.Network.HAProxy as HA


------------------------------------------------------------------------------
bindSocket :: ByteString -> Int -> IO Socket
bindSocket = bindSocketImpl setSocketOption N.bindSocket listen
{-# INLINE bindSocket #-}


------------------------------------------------------------------------------
bindSocketImpl
    :: (Socket -> SocketOption -> Int -> IO ()) -- ^ mock setSocketOption
    -> (Socket -> N.SockAddr -> IO ())          -- ^ bindSocket
    -> (Socket -> Int -> IO ())                 -- ^ listen
    -> ByteString
    -> Int
    -> IO Socket
bindSocketImpl _setSocketOption _bindSocket _listen bindAddr bindPort = do
    (family, addr) <- getSockAddr bindPort bindAddr
    bracketOnError (socket family N.Stream 0) N.close $ \sock -> do
        _setSocketOption sock ReuseAddr 1
        _setSocketOption sock NoDelay 1
        _bindSocket sock addr
        _listen sock 150
        return $! sock


------------------------------------------------------------------------------
-- TODO(greg): move buffer size configuration into config
bUFSIZ :: Int
bUFSIZ = 4064


------------------------------------------------------------------------------
haProxyAcceptFunc :: Socket     -- ^ bound socket
                  -> AcceptFunc
haProxyAcceptFunc boundSocket = AcceptFunc $ \restore -> do
    (sock, _)                <- restore $ accept boundSocket
    (readEnd, writeEnd)      <- Streams.socketToStreamsWithBufferSize bUFSIZ sock
    localPInfo               <- HA.socketToProxyInfo sock
    pinfo                    <- HA.decodeHAProxyHeaders localPInfo readEnd
    (localPort, localHost)   <- getAddress $ HA.getDestAddr pinfo
    (remotePort, remoteHost) <- getAddress $ HA.getSourceAddr pinfo
    let cleanup              =  Streams.write Nothing writeEnd >> close sock
    return $! ( sendFileFunc sock
              , localHost
              , localPort
              , remoteHost
              , remotePort
              , readEnd
              , writeEnd
              , cleanup
              )


------------------------------------------------------------------------------
httpAcceptFunc :: Socket                     -- ^ bound socket
               -> AcceptFunc
httpAcceptFunc boundSocket = AcceptFunc $ \restore -> do
    (sock, remoteAddr)       <- restore $ accept boundSocket
    localAddr                <- getSocketName sock
    (localPort, localHost)   <- getAddress localAddr
    (remotePort, remoteHost) <- getAddress remoteAddr
    (readEnd, writeEnd)      <- Streams.socketToStreamsWithBufferSize bUFSIZ
                                                                      sock
    let cleanup              =  Streams.write Nothing writeEnd >> close sock
    return $! ( sendFileFunc sock
              , localHost
              , localPort
              , remoteHost
              , remotePort
              , readEnd
              , writeEnd
              , cleanup
              )


------------------------------------------------------------------------------
sendFileFunc :: Socket -> SendFileHandler
#ifdef HAS_SENDFILE
sendFileFunc sock !_ builder fPath offset nbytes = bracket acquire closeFd go
  where
    sockFd    = Fd (fdSocket sock)
    acquire   = openFd fPath ReadOnly Nothing defaultFileFlags
    go fileFd = do sendHeaders builder sockFd
                   sendFile sockFd fileFd offset nbytes


#else
sendFileFunc sock buffer builder fPath offset nbytes =
    Streams.unsafeWithFileAsInputStartingAt (fromIntegral offset) fPath $
            \fileInput0 -> do
        fileInput <- Streams.takeBytes (fromIntegral nbytes) fileInput0 >>=
                     Streams.map fromByteString
        input     <- Streams.fromList [builder] >>=
                     Streams.appendInputStream fileInput
        output    <- Streams.makeOutputStream sendChunk >>=
                     Streams.unsafeBuilderStream (return buffer)
        Streams.connect input output

  where
    sendChunk (Just s) = sendAll sock s
    sendChunk Nothing  = return $! ()
#endif
