{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}

module Snap.Internal.Http.Server.TLS
  ( TLSException
  , initTLS
  , stopTLS
  , bindHttps
  , freePort
  , createSession
  , endSession
  , recv
  , send
  ) where


------------------------------------------------------------------------------
import           Control.Exception
import           Data.ByteString.Char8 (ByteString)
import           Data.Dynamic
import           Foreign.C

import           Snap.Internal.Debug
import           Snap.Internal.Http.Server.Address
import           Snap.Internal.Http.Server.Backend

#ifdef OPENSSL
import           Control.Monad
import qualified Data.ByteString.Char8 as S
import qualified Network.Socket as Socket
import           Network.Socket hiding ( accept
                                       , shutdown
                                       , recv
                                       , recvLen
                                       , send
                                       , socket
                                       )
import           OpenSSL
import           OpenSSL.Session
import qualified OpenSSL.Session as SSL
import           Unsafe.Coerce
#endif


data TLSException = TLSException String
    deriving (Show, Typeable)
instance Exception TLSException

#ifndef OPENSSL

initTLS :: IO ()
initTLS = throwIO $ TLSException "TLS is not supported"

stopTLS :: IO ()
stopTLS = return ()

bindHttps :: ByteString -> Int -> FilePath -> FilePath -> IO ListenSocket
bindHttps _ _ _ _ = throwIO $ TLSException "TLS is not supported"

freePort :: ListenSocket -> IO ()
freePort _ = return ()

createSession :: ListenSocket -> Int -> CInt -> IO () -> IO NetworkSession
createSession _ _ _ _ = throwIO $ TLSException "TLS is not supported"

endSession :: NetworkSession -> IO ()
endSession _ = return ()

send :: IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send _ _ _ _ = return ()

recv :: IO b -> NetworkSession -> IO (Maybe ByteString)
recv _ _ = throwIO $ TLSException "TLS is not supported"

------------------------------------------------------------------------------
#else

initTLS :: IO ()
initTLS = withOpenSSL $ return ()

stopTLS :: IO ()
stopTLS = return ()

bindHttps :: ByteString
          -> Int
          -> FilePath
          -> FilePath
          -> IO ListenSocket
bindHttps bindAddress bindPort cert key = do
    (family, addr) <- getSockAddr bindPort bindAddress
    sock <- Socket.socket family Socket.Stream 0
    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bindSocket sock addr
    Socket.listen sock 150

    ctx <- context
    contextSetPrivateKeyFile ctx key
    contextSetCertificateFile ctx cert
    contextSetDefaultCiphers ctx
    certOK <- contextCheckPrivateKey ctx
    when (not certOK) $ do
        throwIO $ TLSException $ "OpenSSL says that the certificate "
                ++ "doesn't match the private key!"

    return $ ListenHttps sock ctx


freePort :: ListenSocket -> IO ()
freePort (ListenHttps sock _) = Socket.sClose sock
freePort _ = return ()


createSession :: ListenSocket -> Int -> CInt -> IO () -> IO NetworkSession
createSession (ListenHttps _ ctx) recvSize socket _ = do
    csock <- mkSocket socket AF_INET Stream defaultProtocol Connected
    ssl   <- connection ctx csock

    accept ssl
    return $ NetworkSession socket (unsafeCoerce ssl) recvSize
createSession _ _ _ _ = error "can't call createSession on a ListenHttp"


endSession :: NetworkSession -> IO ()
endSession (NetworkSession _ aSSL _) = shutdown ssl Bidirectional
  where
    ssl = unsafeCoerce aSSL


send :: IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send tickleTimeout _ (NetworkSession _ aSSL sz) bs = go bs
  where
    ssl = unsafeCoerce aSSL

    -- I think we have to chop the data into chunks here because HsOpenSSL
    -- won't; of course, blaze-builder may already be doing this for us, but I
    -- don't want to risk it.
    go !s = if S.null s
              then return ()
              else do
                  SSL.write ssl a
                  tickleTimeout
                  go b

      where
        (a,b) = S.splitAt sz s


recv :: IO b -> NetworkSession -> IO (Maybe ByteString)
recv _ (NetworkSession _ aSSL recvLen) = do
    b <- SSL.read ssl recvLen
    if S.null b then return Nothing else return $ Just b
  where
    ssl = unsafeCoerce aSSL


#endif
