{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Snap.Internal.Http.Server.GnuTLS
  ( GnuTLSException(..)
  , initTLS
  , stopTLS
  , bindHttps
  , freePort
  , createSession
  , endSession
  , recv
  , send
  ) where

import           Control.Exception
import           Data.ByteString (ByteString)
import           Data.Dynamic
import           Foreign.C

import           Snap.Internal.Http.Server.Backend

#ifdef GNUTLS
import           Control.Monad (liftM)
import qualified Data.ByteString as B
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.ByteString.Internal (w2c)
import           Foreign
import qualified Network.Socket as Socket
#endif

data GnuTLSException = GnuTLSException String
    deriving (Show, Typeable)
instance Exception GnuTLSException

#ifndef GNUTLS

initTLS :: IO ()
initTLS = throwIO $ GnuTLSException "TLS is not supported"

stopTLS :: IO ()
stopTLS = return ()

bindHttps :: ByteString -> Int -> FilePath -> FilePath -> IO ListenSocket
bindHttps _ _ _ _ = throwIO $ GnuTLSException "TLS is not supported"

freePort :: ListenSocket -> IO ()
freePort _ = return ()

createSession :: ListenSocket -> Int -> CInt -> IO () -> IO NetworkSession
createSession _ _ _ _ = throwIO $ GnuTLSException "TLS is not supported"

endSession :: NetworkSession -> IO ()
endSession _ = return ()

send :: IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send _ _ _ _ = return ()

recv :: IO b -> NetworkSession -> IO (Maybe ByteString)
recv _ _ = throwIO $ GnuTLSException "TLS is not supported"

#else


-------------------------------------------------------------------------------
--- Init
initTLS :: IO ()
initTLS = gnutls_set_threading_helper >> throwErrorIf "TLS init" gnutls_global_init

stopTLS :: IO ()
stopTLS = gnutls_global_deinit

-------------------------------------------------------------------------------
-- Bind ssl port
bindHttps :: ByteString
          -> Int
          -> FilePath
          -> FilePath
          -> IO ListenSocket
bindHttps bindAddress bindPort cert key = do
    sock <- Socket.socket Socket.AF_INET Socket.Stream 0
    addr <- getHostAddr bindPort bindAddress
    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bindSocket sock addr
    Socket.listen sock 150

    creds <- loadCredentials cert key
    dh <- regenerateDHParam creds

    return $ ListenHttps sock (castPtr creds) (castPtr dh)

loadCredentials :: FilePath       --- ^ Path to certificate
                -> FilePath       --- ^ Path to key
                -> IO (Ptr GnuTLSCredentials)
loadCredentials cert key = alloca $ \cPtr -> do
    throwErrorIf "TLS allocate" $ gnutls_certificate_allocate_credentials cPtr
    creds <- peek cPtr

    withCString cert $ \certstr -> withCString key $ \keystr ->
        throwErrorIf "TLS set Certificate" $ gnutls_certificate_set_x509_key_file creds certstr keystr gnutls_x509_fmt_pem

    return creds

regenerateDHParam :: Ptr GnuTLSCredentials -> IO (Ptr GnuTLSDHParam)
regenerateDHParam creds = alloca $ \dhptr -> do
    throwErrorIf "TLS allocate" $ gnutls_dh_params_init dhptr
    dh <- peek dhptr
    throwErrorIf "TLS DHParm" $ gnutls_dh_params_generate2 dh 1024
    gnutls_certificate_set_dh_params creds dh
    return dh

-------------------------------------------------------------------------------

freePort :: ListenSocket -> IO ()
freePort (ListenHttps _ creds dh) = do
    gnutls_certificate_free_credentials $ castPtr creds
    gnutls_dh_params_deinit $ castPtr dh
freePort _ = return ()

createSession :: ListenSocket -> Int -> CInt -> IO () -> IO NetworkSession
createSession (ListenHttps _ creds _) recvSize socket on_block = alloca $ \sPtr -> do
    throwErrorIf "TLS alloacte" $ gnutls_init sPtr 1
    session <- peek sPtr
    throwErrorIf "TLS session" $ gnutls_credentials_set session 1 $ castPtr creds
    throwErrorIf "TLS session" $ gnutls_set_default_priority session
    gnutls_certificate_send_x509_rdn_sequence session 1
    gnutls_session_enable_compatibility_mode session

    buffer <- mallocBytes $ fromIntegral recvSize
    let s = NetworkSession socket (castPtr session) buffer $ fromIntegral recvSize

    gnutls_transport_set_ptr session $ intPtrToPtr $ fromIntegral $ socket

    handshake s on_block

    return s
createSession _ _ _ _ = error "Invalid socket"

endSession :: NetworkSession -> IO ()
endSession (NetworkSession _ session buffer _) = do
    throwErrorIf "TLS bye" $ gnutls_bye (castPtr session) 1 `finally` do
        gnutls_deinit $ castPtr session
        free buffer

handshake :: NetworkSession -> IO () -> IO ()
handshake s@(NetworkSession { _session = session}) on_block = do
    rc <- gnutls_handshake $ castPtr session
    case rc of
        x | x >= 0         -> return ()
          | isIntrCode x   -> handshake s on_block
          | isAgainCode x  -> on_block >> handshake s on_block
          | otherwise      -> throwError "TLS handshake" rc

send :: IO () -> IO () -> NetworkSession -> ByteString -> IO ()
send tickleTimeout onBlock (NetworkSession { _session = session}) bs =
     unsafeUseAsCStringLen bs $ uncurry loop
  where 
    loop ptr len = do
        sent <- gnutls_record_send (castPtr session) ptr $ fromIntegral len
        let sent' = fromIntegral sent
        case sent' of
            x | x == 0 || x == len -> return ()
              | x > 0 && x < len   -> tickleTimeout >> loop (plusPtr ptr sent') (len - sent')
              | isIntrCode x       -> loop ptr len
              | isAgainCode x      -> onBlock >> loop ptr len
              | otherwise          -> throwError "TLS send" $ fromIntegral sent'

{- I originally wrote recv to use mallocBytes and unsafePackCStringFinalizer to achieve zero-copy.
   The downside to that method is we might waste memory if a malicious adversary only sends us a few bytes,
   since the entire buffer won't be freed until the ByteString is collected.  Thus I use packCStringLen which
   makes a copy.  Perhaps in the future the recv function could be changed to use unsafePackCStringFinalizer if
   the buffer is at least 3/4 full and packCStringLen otherwise or something like that -} 
recv :: IO b -> NetworkSession -> IO (Maybe ByteString)
recv onBlock (NetworkSession _ session recvBuf recvLen) = loop
  where
    loop = do
        size <- gnutls_record_recv (castPtr session) recvBuf recvLen
        let size' = fromIntegral size
        case size' of
            x | x == 0        -> return Nothing
              | x > 0         -> liftM Just $ B.packCStringLen (recvBuf, x)
              | isIntrCode x  -> loop
              | isAgainCode x -> onBlock >> loop
              | otherwise     -> (throwError "TLS recv" $ fromIntegral size') >> return Nothing

throwError :: String -> ReturnCode -> IO ()
throwError prefix rc = gnutls_strerror rc >>= peekCString >>= throwIO . GnuTLSException . (prefix'++)
    where prefix' = prefix ++ "<" ++ show rc ++ ">: "

throwErrorIf :: String -> IO ReturnCode -> IO ()
throwErrorIf prefix action = do
    rc <- action
    if (rc < 0)
        then throwError prefix rc
        else return ()

isAgainCode :: (Integral a) => a -> Bool
isAgainCode x = (fromIntegral x) == (-28 :: Int)

isIntrCode :: (Integral a) => a -> Bool
isIntrCode x = (fromIntegral x) == (-52 :: Int)

getHostAddr :: Int
            -> ByteString
            -> IO Socket.SockAddr
getHostAddr p s = do
    h <- if s == "*"
          then return Socket.iNADDR_ANY
          else Socket.inet_addr (map w2c . B.unpack $ s)

    return $ Socket.SockAddrInet (fromIntegral p) h

-- Types

newtype ReturnCode = ReturnCode CInt
    deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

data GnuTLSCredentials
data GnuTLSSession
data GnuTLSDHParam

-- Global init/errors

foreign import ccall safe "gnutls_set_threading_helper"
   gnutls_set_threading_helper :: IO ()

foreign import ccall safe "gnutls/gnutls.h gnutls_global_init"
    gnutls_global_init :: IO ReturnCode

foreign import ccall safe "gnutls/gnutls.h gnutls_global_deinit"
    gnutls_global_deinit :: IO ()

foreign import ccall safe "gnutls/gnutls.h gnutls_strerror"
    gnutls_strerror :: ReturnCode -> IO CString

-- Sessions.  All functions here except handshake and bye just
-- allocate memory or update members of structures, so they are ok with
-- unsafe ccall.

foreign import ccall unsafe "gnutls/gnutls.h gnutls_init"
    gnutls_init :: Ptr (Ptr GnuTLSSession) -> CInt -> IO ReturnCode

foreign import ccall unsafe "gnutls/gnutls.h gnutls_deinit"
    gnutls_deinit :: Ptr GnuTLSSession -> IO ()

foreign import ccall safe "gnutls/gnutls.h gnutls_handshake"
    gnutls_handshake :: Ptr GnuTLSSession -> IO ReturnCode

foreign import ccall safe "gnutls/gnutls.h gnutls_bye"
    gnutls_bye :: Ptr GnuTLSSession -> CInt -> IO ReturnCode

foreign import ccall unsafe "gnutls/gnutls.h gnutls_set_default_priority"
    gnutls_set_default_priority :: Ptr GnuTLSSession -> IO ReturnCode

foreign import ccall unsafe "gnutls/gnutls.h gnutls_session_enable_compatibility_mode"
    gnutls_session_enable_compatibility_mode :: Ptr GnuTLSSession -> IO ()

foreign import ccall unsafe "gnutls/gnutls.h gnutls_certificate_send_x509_rdn_sequence"
    gnutls_certificate_send_x509_rdn_sequence :: Ptr GnuTLSSession -> CInt -> IO ()

-- Certificates.  Perhaps these could be unsafe but they are not performance critical,
-- since they are called only once during server startup.

foreign import ccall safe "gnutls/gnutls.h gnutls_certificate_allocate_credentials"
    gnutls_certificate_allocate_credentials :: Ptr (Ptr GnuTLSCredentials) -> IO ReturnCode

foreign import ccall safe "gnutls/gnutls.h gnutls_certificate_free_credentials"
    gnutls_certificate_free_credentials :: Ptr GnuTLSCredentials -> IO ()

gnutls_x509_fmt_pem :: CInt
gnutls_x509_fmt_pem = 1

foreign import ccall safe "gnutls/gnutls.h gnutls_certificate_set_x509_key_file"
    gnutls_certificate_set_x509_key_file :: Ptr GnuTLSCredentials -> CString -> CString -> CInt -> IO ReturnCode


-- Credentials.  This is ok as unsafe because it just sets members in the session structure.

foreign import ccall unsafe "gnutls/gnutls.h gnutls_credentials_set"
    gnutls_credentials_set :: Ptr GnuTLSSession -> CInt -> Ptr a -> IO ReturnCode

-- Records.  These are marked unsafe because they are very performance critical.  Since
-- we are using non-blocking sockets send and recv will not block.

foreign import ccall unsafe "gnutls/gnutls.h gnutls_transport_set_ptr"
    gnutls_transport_set_ptr :: Ptr GnuTLSSession -> Ptr a -> IO ()

foreign import ccall unsafe "gnutls/gnutls.h gnutls_record_recv"
    gnutls_record_recv :: Ptr GnuTLSSession -> Ptr a -> CSize -> IO CSize

foreign import ccall unsafe "gnutls/gnutls.h gnutls_record_send"
    gnutls_record_send :: Ptr GnuTLSSession -> Ptr a -> CSize -> IO CSize

-- DHParam.  Perhaps these could be unsafe but they are not performance critical.

foreign import ccall safe "gnutls/gnutls.h gnutls_dh_params_init"
    gnutls_dh_params_init :: Ptr (Ptr GnuTLSDHParam) -> IO ReturnCode

foreign import ccall safe "gnutls/gnutls.h gnutls_dh_params_deinit"
    gnutls_dh_params_deinit :: Ptr GnuTLSDHParam -> IO ()

foreign import ccall safe "gnutls/gnutls.h gnutls_dh_params_generate2"
    gnutls_dh_params_generate2 :: Ptr GnuTLSDHParam -> CUInt -> IO ReturnCode

foreign import ccall safe "gnutls/gnutls.h gnutls_certificate_set_dh_params"
    gnutls_certificate_set_dh_params :: Ptr GnuTLSCredentials -> Ptr GnuTLSDHParam -> IO ()

#endif
