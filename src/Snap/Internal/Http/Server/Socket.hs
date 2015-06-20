{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snap.Internal.Http.Server.Socket
  ( bindSocket
  , bindSocketImpl
  , bindUnixSocket
  , httpAcceptFunc
  , haProxyAcceptFunc
  , sendFileFunc
  , acceptAndInitialize
  ) where

------------------------------------------------------------------------------
import           Control.Exception                 (bracketOnError, finally, throwIO)
import           Control.Monad                     (when)
import           Data.Bits                         (complement, (.&.))
import           Data.ByteString.Char8             (ByteString)
import           Network.Socket                    (Socket, SocketOption (NoDelay, ReuseAddr), accept, close, getSocketName, listen, setSocketOption, socket)
import qualified Network.Socket                    as N
#ifdef HAS_SENDFILE
import           Network.Socket                    (fdSocket)
import           System.Posix.IO                   (OpenMode (..), closeFd, defaultFileFlags, openFd)
import           System.Posix.Types                (Fd (..))
import           System.SendFile                   (sendFile, sendHeaders)
#else
import           Data.ByteString.Builder           (byteString)
import           Data.ByteString.Builder.Extra     (flush)
import           Network.Socket.ByteString         (sendAll)
#endif
#ifdef HAS_UNIX_SOCKETS
import           Control.Exception                 (bracket)
import qualified Control.Exception                 as E (catch)
import           System.FilePath                   (isRelative)
import           System.IO.Error                   (isDoesNotExistError)
import           System.Posix.Files                (accessModes, removeLink, setFileCreationMask)
#endif

------------------------------------------------------------------------------
import qualified System.IO.Streams                 as Streams
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Address (AddressNotSupportedException (..), getAddress, getSockAddr)
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

bindUnixSocket :: Maybe Int -> String -> IO Socket
#if HAS_UNIX_SOCKETS
bindUnixSocket mode path = do
   when (isRelative path) $
      throwIO $ AddressNotSupportedException
                $! "Refusing to bind unix socket to non-absolute path: " ++ path

   bracketOnError (socket N.AF_UNIX N.Stream 0) N.close $ \sock -> do
      E.catch (removeLink path) $ \e -> when (not $ isDoesNotExistError e) $ throwIO e
      case mode of
         Nothing -> N.bindSocket sock (N.SockAddrUnix path)
         Just mode' -> bracket (setFileCreationMask $ modeToMask mode')
                              setFileCreationMask
                              (const $ N.bindSocket sock (N.SockAddrUnix path))
      N.listen sock 150
      return $! sock
   where
     modeToMask p = accessModes .&. complement (fromIntegral p)
#else
bindUnixSocket _ path = throwIO (AddressNotSupportedException $ "unix:" ++ path)
#endif

------------------------------------------------------------------------------
-- TODO(greg): move buffer size configuration into config
bUFSIZ :: Int
bUFSIZ = 4064


------------------------------------------------------------------------------
acceptAndInitialize :: Socket        -- ^ bound socket
                    -> (forall b . IO b -> IO b)
                    -> ((Socket, N.SockAddr) -> IO a)
                    -> IO a
acceptAndInitialize boundSocket restore f =
    bracketOnError (restore $ accept boundSocket)
                   (close . fst)
                   f


------------------------------------------------------------------------------
haProxyAcceptFunc :: Socket     -- ^ bound socket
                  -> AcceptFunc
haProxyAcceptFunc boundSocket =
    AcceptFunc $ \restore ->
    acceptAndInitialize boundSocket restore $ \(sock, saddr) -> do
        (readEnd, writeEnd)      <- Streams.socketToStreamsWithBufferSize
                                        bUFSIZ sock
        localPInfo               <- HA.socketToProxyInfo sock saddr
        pinfo                    <- HA.decodeHAProxyHeaders localPInfo readEnd
        (localPort, localHost)   <- getAddress $ HA.getDestAddr pinfo
        (remotePort, remoteHost) <- getAddress $ HA.getSourceAddr pinfo
        let cleanup              =  Streams.write Nothing writeEnd
                                        `finally` close sock
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
httpAcceptFunc boundSocket =
    AcceptFunc $ \restore ->
    acceptAndInitialize boundSocket restore $ \(sock, remoteAddr) -> do
        localAddr                <- getSocketName sock
        (localPort, localHost)   <- getAddress localAddr
        (remotePort, remoteHost) <- getAddress remoteAddr
        (readEnd, writeEnd)      <- Streams.socketToStreamsWithBufferSize bUFSIZ
                                                                          sock
        let cleanup              =  Streams.write Nothing writeEnd
                                      `finally` close sock
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
                     Streams.map byteString
        input     <- Streams.fromList [builder] >>=
                     flip Streams.appendInputStream fileInput
        output    <- Streams.makeOutputStream sendChunk >>=
                     Streams.unsafeBuilderStream (return buffer)
        Streams.supply input output
        Streams.write (Just flush) output

  where
    sendChunk (Just s) = sendAll sock s
    sendChunk Nothing  = return $! ()
#endif
