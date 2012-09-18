{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
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
------------------------------------------------------------------------------
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
import           Prelude hiding (catch)
import           Unsafe.Coerce
import           Snap.Internal.Http.Server.Address
#endif
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Backend


------------------------------------------------------------------------------
data TLSException = TLSException String
  deriving (Show, Typeable)
instance Exception TLSException


#ifndef OPENSSL
------------------------------------------------------------------------------
initTLS :: IO ()
initTLS = throwIO $ TLSException "TLS is not supported"


------------------------------------------------------------------------------
stopTLS :: IO ()
stopTLS = return ()


------------------------------------------------------------------------------
bindHttps :: ByteString -> Int -> FilePath -> FilePath -> IO ListenSocket
bindHttps _ _ _ _ = throwIO $ TLSException "TLS is not supported"


------------------------------------------------------------------------------
freePort :: ListenSocket -> IO ()
freePort _ = return ()


------------------------------------------------------------------------------
createSession :: ListenSocket -> Int -> CInt -> IO () -> IO NetworkSession
createSession _ _ _ _ = throwIO $ TLSException "TLS is not supported"


------------------------------------------------------------------------------
endSession :: NetworkSession -> IO ()
endSession _ = return ()


------------------------------------------------------------------------------
send :: IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send _ _ _ _ = return ()


------------------------------------------------------------------------------
recv :: IO b -> NetworkSession -> IO (Maybe ByteString)
recv _ _ = throwIO $ TLSException "TLS is not supported"


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
          -> FilePath
          -> IO ListenSocket
bindHttps bindAddress bindPort cert key = do
    (family, addr) <- getSockAddr bindPort bindAddress
    sock           <- Socket.socket family Socket.Stream 0

    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bindSocket sock addr
    Socket.listen sock 150

    ctx <- context
    contextSetPrivateKeyFile  ctx key
    contextSetCertificateFile ctx cert
    contextSetDefaultCiphers  ctx

    certOK <- contextCheckPrivateKey ctx
    when (not certOK) $ throwIO $ TLSException certificateError
    return $! ListenHttps sock ctx

  where
    certificateError = "OpenSSL says that the certificate " ++
                       "doesn't match the private key!"


------------------------------------------------------------------------------
freePort :: ListenSocket -> IO ()
freePort (ListenHttps sock ctx) = do
    Socket.sClose sock
    -- FIXME(greg): this is a desperation tactic. Better to find and kill the
    -- hypothesized liveness bug in HsOpenSSL.
    --
    -- Touch the context so that the garbage collector doesn't zap it.
    !_ <- contextGetCAStore ctx
    return $! ()
freePort _ = return ()


------------------------------------------------------------------------------
data SessionContext = SessionContext SSL SSLContext

------------------------------------------------------------------------------
createSession :: ListenSocket -> Int -> CInt -> IO () -> IO NetworkSession
createSession (ListenHttps _ ctx) recvSize socket _ = do
    csock <- mkSocket socket AF_INET Stream defaultProtocol Connected
    handle (\(e::SomeException) -> Socket.sClose csock >> throwIO e) $ do
        ssl <- connection ctx csock
        accept ssl
        let sctx = SessionContext ssl ctx
        return $! NetworkSession socket (unsafeCoerce sctx) recvSize
createSession _ _ _ _ = error "can't call createSession on a ListenHttp"


------------------------------------------------------------------------------
endSession :: NetworkSession -> IO ()
endSession (NetworkSession _ aSSL _) = do
    let (SessionContext ssl ctx) = unsafeCoerce aSSL
    shutdown ssl Unidirectional

    -- FIXME(greg): fix this properly, see above
    -- Touch the context so that the garbage collector doesn't zap it.
    !_ <- contextGetCAStore ctx
    return $! ()

------------------------------------------------------------------------------
send :: IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send tickleTimeout _ (NetworkSession _ aSSL sz) bs = go bs
  where
    (SessionContext ssl ctx) = unsafeCoerce aSSL

    -- I think we have to chop the data into chunks here because HsOpenSSL
    -- won't; of course, blaze-builder may already be doing this for us, but I
    -- don't want to risk it.
    go !s = if S.null s
              then return ()
              else do
                SSL.write ssl a
                tickleTimeout
                -- FIXME(greg): fix this properly, see above
                -- Touch the context so that the garbage collector doesn't zap
                -- it.
                !_ <- contextGetCAStore ctx
                go b
      where
        (a,b) = S.splitAt sz s


------------------------------------------------------------------------------
recv :: IO b -> NetworkSession -> IO (Maybe ByteString)
recv _ (NetworkSession _ aSSL recvLen) = do
    b  <- SSL.read ssl recvLen
    !_ <- contextGetCAStore ctx
    return $! if S.null b then Nothing else Just b
  where
    (SessionContext ssl ctx) = unsafeCoerce aSSL

#endif
