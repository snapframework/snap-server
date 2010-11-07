{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Internal.Http.Server.HttpPort
  ( bindHttp
  , createSession
  , endSession
  , recv
  , send
  ) where

import           Control.Monad (liftM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Internal (w2c)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Foreign
import           Foreign.C
import           Network.Socket hiding (recv, send)

import           Snap.Internal.Http.Server.Backend

bindHttp :: ByteString -> Int -> IO ListenSocket
bindHttp bindAddr bindPort = do
    sock <- socket AF_INET Stream 0
    addr <- getHostAddr bindPort bindAddr
    setSocketOption sock ReuseAddr 1
    bindSocket sock addr
    listen sock 150
    return $ ListenHttp sock

createSession :: Int -> CInt -> IO () -> IO NetworkSession
createSession buffSize s _ = do
    buffer <- mallocBytes $ fromIntegral buffSize
    return $ NetworkSession s nullPtr buffer $ fromIntegral buffSize

endSession :: NetworkSession -> IO ()
endSession (NetworkSession {_recvBuffer = buff}) = free buff

recv :: IO () -> NetworkSession -> IO (Maybe ByteString)
recv onBlock (NetworkSession s _ buff buffSize) = do
    sz <- throwErrnoIfMinus1RetryMayBlock
              "recv"
              (c_read s buff buffSize)
              onBlock
    if sz == 0
        then return Nothing
        else liftM Just $ B.packCStringLen (buff, fromIntegral sz)

send :: IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send tickleTimeout onBlock (NetworkSession s _ _ _) bs =
    unsafeUseAsCStringLen bs $ uncurry loop
  where loop ptr len = do
          sent <- throwErrnoIfMinus1RetryMayBlock
                    "send"
                    (c_write s ptr $ fromIntegral len)
                    onBlock

          let sent' = fromIntegral sent
          if sent' < len
             then tickleTimeout >> loop (plusPtr ptr sent') (len - sent')
             else return ()

getHostAddr :: Int
            -> ByteString
            -> IO SockAddr
getHostAddr p s = do
    h <- if s == "*"
          then return iNADDR_ANY
          else inet_addr (map w2c . B.unpack $ s)

    return $ SockAddrInet (fromIntegral p) h

foreign import ccall safe "unistd.h read" c_read :: CInt -> Ptr a -> CSize -> IO (CSize)
foreign import ccall safe "unistd.h write" c_write :: CInt -> Ptr a -> CSize -> IO (CSize)
