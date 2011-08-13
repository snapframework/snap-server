{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Snap.Internal.Http.Server.HttpPort
  ( bindHttp
  , createSession
  , endSession
  , recv
  , send
  ) where


------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Internal (w2c)
import           Foreign
import           Foreign.C
import           Network.Socket hiding (recv, send)
import           Unsafe.Coerce

#ifdef PORTABLE
import qualified Network.Socket.ByteString as SB
#else
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BI
#endif

import           Snap.Internal.Debug
import           Snap.Internal.Http.Server.Backend

------------------------------------------------------------------------------
bindHttp :: ByteString -> Int -> IO ListenSocket
bindHttp bindAddr bindPort = do
    sock <- socket AF_INET Stream 0
    addr <- getHostAddr bindPort bindAddr
    debug $ "bindHttp: binding port " ++ show addr
    setSocketOption sock ReuseAddr 1
    bindSocket sock addr
    listen sock 150
    debug $ "bindHttp: bound socket " ++ show sock
    return $ ListenHttp sock


------------------------------------------------------------------------------
getHostAddr :: Int
            -> ByteString
            -> IO SockAddr
getHostAddr p s = do
    h <- if s == "*"
          then return iNADDR_ANY
          else inet_addr (map w2c . B.unpack $ s)

    return $ SockAddrInet (fromIntegral p) h


------------------------------------------------------------------------------
createSession :: Int -> CInt -> IO () -> IO NetworkSession
createSession buffSize s _ =
    return $ NetworkSession s (unsafeCoerce ()) $ fromIntegral buffSize


------------------------------------------------------------------------------
endSession :: NetworkSession -> IO ()
endSession _ = return ()

#ifdef PORTABLE

------------------------------------------------------------------------------
recv :: Socket -> IO () -> NetworkSession -> IO (Maybe ByteString)
recv sock _ (NetworkSession { _recvLen = s }) = do
    bs <- SB.recv sock (fromIntegral s)
    if B.null bs
        then return Nothing
        else return $ Just bs


------------------------------------------------------------------------------
send :: Socket -> IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send sock tickle _ _ bs = SB.sendAll sock bs >> tickle

#else

------------------------------------------------------------------------------
recv :: IO () -> NetworkSession -> IO (Maybe ByteString)
recv onBlock (NetworkSession s _ buffSize) = do
    fp <- BI.mallocByteString $ fromEnum buffSize
    sz <- withForeignPtr fp $ \p ->
              throwErrnoIfMinus1RetryMayBlock
                  "recv"
                  (c_read s p $ toEnum buffSize)
                  onBlock

    if sz == 0
      then return Nothing
      else return $ Just $ BI.fromForeignPtr fp 0 $ fromEnum sz


------------------------------------------------------------------------------
send :: IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send tickleTimeout onBlock (NetworkSession s _ _) bs =
    BI.unsafeUseAsCStringLen bs $ uncurry loop
  where loop ptr len = do
          sent <- throwErrnoIfMinus1RetryMayBlock
                    "send"
                    (c_write s ptr $ toEnum len)
                    onBlock

          let sent' = fromIntegral sent
          if sent' < len
             then tickleTimeout >> loop (plusPtr ptr sent') (len - sent')
             else return ()


------------------------------------------------------------------------------
foreign import ccall unsafe "unistd.h read" c_read
    :: CInt -> Ptr a -> CSize -> IO (CSize)
foreign import ccall unsafe "unistd.h write" c_write
    :: CInt -> Ptr a -> CSize -> IO (CSize)

#endif
