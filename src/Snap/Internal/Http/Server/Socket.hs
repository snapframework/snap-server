module Snap.Internal.Http.Server.Socket
  ( bindHttp
  ) where

import           Data.ByteString.Char8             (ByteString)
import           Network.Socket                    (Socket,
                                                    SocketOption (ReuseAddr),
                                                    SocketType (Stream),
                                                    bindSocket, listen,
                                                    setSocketOption, socket)
import           Snap.Internal.Http.Server.Address (getSockAddr)


------------------------------------------------------------------------------
bindHttp :: ByteString -> Int -> IO Socket
bindHttp bindAddr bindPort = do
    (family, addr) <- getSockAddr bindPort bindAddr
    sock           <- socket family Stream 0

    setSocketOption sock ReuseAddr 1
    bindSocket sock addr
    listen sock 150
    return $! sock
