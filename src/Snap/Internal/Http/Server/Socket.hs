{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snap.Internal.Http.Server.Socket
  ( bindHttp
  , httpAcceptFunc
  ) where

import           Blaze.ByteString.Builder           (fromByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import           Control.Exception                  (AsyncException,
                                                     Exception, Handler (..),
                                                     IOException,
                                                     SomeException, catches,
                                                     throwIO)
import           Control.Monad                      (liftM)
import           Data.ByteString.Char8              (ByteString)
import           Data.Monoid                        (mconcat)
import           Network.Socket                     (Socket, SocketOption (NoDelay, ReuseAddr),
                                                     SocketType (Stream),
                                                     accept, bindSocket,
                                                     close, getSocketName,
                                                     listen, setSocketOption,
                                                     socket)
import           Snap.Internal.Http.Server.Address  (getAddress, getSockAddr)
import           Snap.Internal.Http.Server.Types    (AcceptFunc,
                                                     SendFileHandler,
                                                     ServerConfig (_logError))
import qualified System.IO.Streams.Network          as Streams
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
getLocalAddress :: Socket -> IO ByteString
getLocalAddress sock = getSocketName sock >>= liftM snd . getAddress


------------------------------------------------------------------------------
sendfileHandler :: Socket -> SendFileHandler
sendfileHandler _ = error "not yet implemented"


------------------------------------------------------------------------------
httpAcceptFunc :: Socket                     -- ^ bound socket
               -> AcceptFunc hookState
httpAcceptFunc boundSocket serverConfig restore = go

  where
    --------------------------------------------------------------------------
    handlers = [ Handler $ \(e :: IOException)    -> logException e >> go
               , Handler $ \(e :: AsyncException) -> throwIO e
               , Handler $ \(e :: SomeException)  -> logException e >>
                                                     throwIO e
               ]

    --------------------------------------------------------------------------
    logError = _logError serverConfig

    --------------------------------------------------------------------------
    logException :: Exception e => e -> IO ()
    logException e =
        logError $
        mconcat [ fromByteString "got exception in httpAcceptFunc: "
                , fromShow e
                ]

    --------------------------------------------------------------------------
    go = flip catches handlers $ do
        (sock, remoteAddr) <- restore $ accept boundSocket
        localAddr <- getLocalAddress sock
        (remotePort, remoteHost) <- getAddress remoteAddr
        (readEnd, writeEnd) <- Streams.socketToStreams sock
        return $! ( sendfileHandler sock
                  , localAddr
                  , remoteHost
                  , remotePort
                  , readEnd
                  , writeEnd
                  , close sock
                  )
