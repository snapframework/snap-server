{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Internal.Http.Server.TLS
  ( TLSException (..)
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
import qualified Data.ByteString.Char8 as S
------------------------------------------------------------------------------
#ifdef OPENSSL
import           Control.Monad
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
import           Prelude hiding (catch)
import           Unsafe.Coerce
import           Snap.Internal.Http.Server.Address
#endif
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Backend


------------------------------------------------------------------------------
data TLSException = TLSException S.ByteString
  deriving (Show, Typeable)
instance Exception TLSException


#ifndef OPENSSL
------------------------------------------------------------------------------
sslNotSupportedException :: TLSException
sslNotSupportedException = TLSException $ S.concat [
    "This version of snap-server was not built with SSL "
  , "support.\n"
  , "Please compile snap-server with -fopenssl to enable it."
  ]

------------------------------------------------------------------------------
initTLS :: IO ()
initTLS = throwIO sslNotSupportedException


------------------------------------------------------------------------------
stopTLS :: IO ()
stopTLS = return ()


------------------------------------------------------------------------------
bindHttps :: ByteString -> Int -> FilePath -> Bool -> FilePath -> IO ListenSocket
bindHttps _ _ _ _ _ = throwIO sslNotSupportedException


------------------------------------------------------------------------------
freePort :: ListenSocket -> IO ()
freePort _ = return ()


------------------------------------------------------------------------------
createSession :: ListenSocket -> Int -> CInt -> IO () -> IO NetworkSession
createSession _ _ _ _ = throwIO sslNotSupportedException


------------------------------------------------------------------------------
endSession :: NetworkSession -> IO ()
endSession _ = return ()


------------------------------------------------------------------------------
send :: IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send _ _ _ _ = return ()


------------------------------------------------------------------------------
recv :: IO b -> NetworkSession -> IO (Maybe ByteString)
recv _ _ = throwIO sslNotSupportedException


#else
------------------------------------------------------------------------------
initTLS :: IO ()
initTLS = withOpenSSL $ return ()


------------------------------------------------------------------------------
stopTLS :: IO ()
stopTLS = return ()


------------------------------------------------------------------------------
bindHttps :: ByteString
          -> Int
          -> FilePath
          -> Bool
          -> FilePath
          -> IO ListenSocket
bindHttps bindAddress bindPort cert chainCert key = do
    (family, addr) <- getSockAddr bindPort bindAddress
    sock           <- Socket.socket family Socket.Stream 0

    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bindSocket sock addr
    Socket.listen sock 150

    ctx <- context
    contextSetPrivateKeyFile  ctx key
    if chainCert
      then contextSetCertificateChainFile ctx cert
      else contextSetCertificateFile ctx cert
    contextSetDefaultCiphers  ctx

    certOK <- contextCheckPrivateKey ctx
    when (not certOK) $ throwIO $ TLSException certificateError
    return $! ListenHttps sock ctx

  where
    certificateError =
      "OpenSSL says that the certificate doesn't match the private key!"


------------------------------------------------------------------------------
freePort :: ListenSocket -> IO ()
freePort (ListenHttps sock _) = Socket.sClose sock
freePort _ = return ()


------------------------------------------------------------------------------
createSession :: ListenSocket -> Int -> CInt -> IO () -> IO NetworkSession
createSession (ListenHttps _ ctx) recvSize socket _ = do
    csock <- mkSocket socket AF_INET Stream defaultProtocol Connected
    handle (\(e::SomeException) -> Socket.sClose csock >> throwIO e) $ do
        ssl <- connection ctx csock
        accept ssl
        return $! NetworkSession socket (unsafeCoerce ssl) recvSize
createSession _ _ _ _ = error "can't call createSession on a ListenHttp"


------------------------------------------------------------------------------
endSession :: NetworkSession -> IO ()
endSession (NetworkSession _ aSSL _) =
    shutdown (unsafeCoerce aSSL) Unidirectional


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
recv :: IO b -> NetworkSession -> IO (Maybe ByteString)
recv _ (NetworkSession _ aSSL recvLen) = do
    b <- SSL.read ssl recvLen
    return $! if S.null b then Nothing else Just b
  where
    ssl = unsafeCoerce aSSL

#endif
