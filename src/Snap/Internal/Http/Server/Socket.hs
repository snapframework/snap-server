{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snap.Internal.Http.Server.Socket
  ( bindHttp
  , httpAcceptFunc
  , sendFileFunc
  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8             (ByteString)
import           Network.Socket                    (Socket, SocketOption (NoDelay, ReuseAddr),
                                                    SocketType (Stream),
                                                    accept, bindSocket, close,
                                                    getSocketName, listen,
                                                    setSocketOption, socket)
import           Snap.Internal.Http.Server.Address (getAddress, getSockAddr)
import           Snap.Internal.Http.Server.Types   (AcceptFunc (..),
                                                    SendFileHandler)
import qualified System.IO.Streams                 as Streams
#ifndef PORTABLE
import           Control.Exception                 (bracket)
import           Network.Socket                    (fdSocket)
import           System.Posix.IO                   (OpenMode (..), closeFd,
                                                    defaultFileFlags, openFd)
import           System.Posix.Types                (Fd (..))
import           System.SendFile                   (sendFile, sendHeaders)
#else
import           Blaze.ByteString.Builder          (fromByteString)
import           Network.Socket.ByteString         (sendAll)
import qualified System.IO.Streams                 as Streams
#endif
------------------------------------------------------------------------------


------------------------------------------------------------------------------
bindHttp :: ByteString -> Int -> IO Socket
bindHttp bindAddr bindPort = do
    (family, addr) <- getSockAddr bindPort bindAddr
    sock           <- socket family Stream 0

    setSocketOption sock ReuseAddr 1
    setSocketOption sock NoDelay 1
    bindSocket sock addr
    listen sock 150
    return $! sock


------------------------------------------------------------------------------
-- TODO(greg): move buffer size configuration into config
bUFSIZ :: Int
bUFSIZ = 4064


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
#ifndef PORTABLE
sendFileFunc sock !_ builder fPath offset nbytes = bracket acquire closeFd go
  where
    sockFd    = Fd (fdSocket sock)
    acquire   = openFd fPath ReadOnly Nothing defaultFileFlags
    go fileFd = do sendHeaders builder sockFd
                   sendFile sockFd fileFd offset nbytes


#else
sendFileFunc sock buffer builder fPath offset nbytes =
    Streams.unsafeWithFileAsInputStartingAt offset fPath $ \fileInput0 -> do
        fileInput <- Streams.takeBytes nbytes fileInput0 >>=
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
