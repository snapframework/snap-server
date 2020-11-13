{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Internal.Http.Server.TLS
  ( TLSException(..)
  , withTLS
  , bindHttps
  , httpsAcceptFunc
  , sendFileFunc
  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Char8             as S
import           Data.Typeable                     (Typeable)
import           Network.Socket                    (Socket)
#ifdef OPENSSL
import           Control.Exception                 (Exception, bracketOnError, finally, onException, throwIO)
import           Control.Monad                     (when)
import           Data.ByteString.Builder           (byteString)
import qualified Snap.Internal.Http.Server.Socket  as Socket
import           OpenSSL                           (withOpenSSL)
import           OpenSSL.Session                   (SSL, SSLContext)
import qualified OpenSSL.Session                   as SSL
import           Prelude                           (Bool, FilePath, IO, Int, Maybe (..), Monad (..), Show, flip, fromIntegral, not, ($), ($!))
import           Snap.Internal.Http.Server.Address (getAddress)
import           Snap.Internal.Http.Server.Socket  (acceptAndInitialize, bindSocket)
import qualified System.IO.Streams                 as Streams
import qualified System.IO.Streams.SSL             as SStreams

#else
import           Control.Exception                 (Exception, throwIO)
import           Prelude                           (Bool, FilePath, IO, Int, Show, id, ($))
#endif
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Types   (AcceptFunc (..), SendFileHandler)
------------------------------------------------------------------------------

data TLSException = TLSException S.ByteString
  deriving (Show, Typeable)
instance Exception TLSException

#ifndef OPENSSL
type SSLContext = ()
type SSL = ()

------------------------------------------------------------------------------
sslNotSupportedException :: TLSException
sslNotSupportedException = TLSException $ S.concat [
    "This version of snap-server was not built with SSL "
  , "support.\n"
  , "Please compile snap-server with -fopenssl to enable it."
  ]


------------------------------------------------------------------------------
withTLS :: IO a -> IO a
withTLS = id


------------------------------------------------------------------------------
barf :: IO a
barf = throwIO sslNotSupportedException


------------------------------------------------------------------------------
bindHttps :: ByteString -> Int -> FilePath -> Bool -> FilePath
          -> IO (Socket, SSLContext)
bindHttps _ _ _ _ _ = barf


------------------------------------------------------------------------------
httpsAcceptFunc :: Socket -> SSLContext -> AcceptFunc
httpsAcceptFunc _ _ = AcceptFunc $ \restore -> restore barf


------------------------------------------------------------------------------
sendFileFunc :: SSL -> Socket -> SendFileHandler
sendFileFunc _ _ _ _ _ _ _ = barf


#else
------------------------------------------------------------------------------
withTLS :: IO a -> IO a
withTLS = withOpenSSL


------------------------------------------------------------------------------
bindHttps :: ByteString
          -> Int
          -> FilePath
          -> Bool
          -> FilePath
          -> IO (Socket, SSLContext)
bindHttps bindAddress bindPort cert chainCert key =
    withTLS $
    bracketOnError
        (bindSocket bindAddress bindPort)
        Socket.close
        $ \sock -> do
             ctx <- SSL.context
             SSL.contextSetPrivateKeyFile ctx key
             if chainCert
               then SSL.contextSetCertificateChainFile ctx cert
               else SSL.contextSetCertificateFile ctx cert
             certOK <- SSL.contextCheckPrivateKey ctx
             when (not certOK) $ do
                 throwIO $ TLSException certificateError
             return (sock, ctx)
  where
    certificateError =
      "OpenSSL says that the certificate doesn't match the private key!"


------------------------------------------------------------------------------
httpsAcceptFunc :: Socket
                -> SSLContext
                -> AcceptFunc
httpsAcceptFunc boundSocket ctx =
    AcceptFunc $ \restore ->
    acceptAndInitialize boundSocket restore $ \(sock, remoteAddr) -> do
        localAddr                <- Socket.getSocketName sock
        (localPort, localHost)   <- getAddress localAddr
        (remotePort, remoteHost) <- getAddress remoteAddr
        ssl                      <- restore (SSL.connection ctx sock)

        restore (SSL.accept ssl) `onException` Socket.close sock

        (readEnd, writeEnd) <- SStreams.sslToStreams ssl

        let cleanup = (do Streams.write Nothing writeEnd
                          SSL.shutdown ssl $! SSL.Unidirectional)
                        `finally` Socket.close sock

        return $! ( sendFileFunc ssl
                  , localHost
                  , localPort
                  , remoteHost
                  , remotePort
                  , readEnd
                  , writeEnd
                  , cleanup
                  )


------------------------------------------------------------------------------
sendFileFunc :: SSL -> SendFileHandler
sendFileFunc ssl buffer builder fPath offset nbytes = do
    Streams.unsafeWithFileAsInputStartingAt (fromIntegral offset) fPath $ \fileInput0 -> do
        fileInput <- Streams.takeBytes (fromIntegral nbytes) fileInput0 >>=
                     Streams.map byteString
        input     <- Streams.fromList [builder] >>=
                     flip Streams.appendInputStream fileInput
        output    <- Streams.makeOutputStream sendChunk >>=
                     Streams.unsafeBuilderStream (return buffer)
        Streams.supply input output
        Streams.write Nothing output

  where
    sendChunk (Just s) = SSL.write ssl s
    sendChunk Nothing  = return $! ()
#endif
