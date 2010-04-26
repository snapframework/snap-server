{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.SimpleBackend
( Backend
, BackendTerminatedException
, Connection
, TimeoutException
, debug
, bindIt
, new
, stop
, withConnection
, sendFile
, getReadEnd
, getWriteEnd
, getRemoteAddr
, getRemotePort
, getLocalAddr
, getLocalPort
) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString as B
import           Data.Iteratee.WrappedByteString
import           Data.Typeable
import           Foreign.C.Types
import           GHC.Conc (labelThread, forkOnIO)
import           Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.Socket.SendFile as SF
import           Prelude hiding (catch)
------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Iteratee


data BackendTerminatedException = BackendTerminatedException
   deriving (Typeable)

instance Show BackendTerminatedException where
    show (BackendTerminatedException) = "Backend terminated"

instance Exception BackendTerminatedException


-- foreign import ccall unsafe "set_linger"
--   set_linger :: CInt -> IO ()

foreign import ccall unsafe "set_fd_timeout"
  set_fd_timeout :: CInt -> IO ()


data Backend = Backend
    { _acceptSocket :: Socket }

data Connection = Connection 
    { _socket      :: Socket
    , _remoteAddr  :: ByteString
    , _remotePort  :: Int
    , _localAddr   :: ByteString
    , _localPort   :: Int }


sendFile :: Connection -> FilePath -> IO ()
sendFile c fp = do
    let s = _socket c
    SF.sendFile s fp


bindIt :: ByteString         -- ^ bind address, or \"*\" for all
       -> Int                -- ^ port to bind to
       -> IO Socket
bindIt bindAddress bindPort = do
    sock <- socket AF_INET Stream 0
    addr <- getHostAddr bindPort bindAddress
    setSocketOption sock ReuseAddr 1
    bindSocket sock addr
    listen sock bindPort
    return sock


new :: Socket   -- ^ value you got from bindIt
    -> Int
    -> IO Backend
new sock _ = do
    debug $ "Backend.new: listening"
    return $ Backend sock


stop :: Backend -> IO ()
stop (Backend s) = do
    debug $ "Backend.stop"
    sClose s


data AddressNotSupportedException = AddressNotSupportedException String
   deriving (Typeable)

instance Show AddressNotSupportedException where
    show (AddressNotSupportedException x) = "Address not supported: " ++ x

instance Exception AddressNotSupportedException


withConnection :: Backend -> Int -> (Connection -> IO ()) -> IO ()
withConnection (Backend asock) cpu proc = do
    debug $ "Backend.withConnection: calling accept()"
    (sock,addr) <- accept asock

    let fd = fdSocket sock
    -- set linger
    --set_linger fd
    set_fd_timeout fd

    debug $ "Backend.withConnection: accepted connection"
    debug $ "Backend.withConnection: remote: " ++ show addr

    (port,host) <-
        case addr of
          SockAddrInet p h -> do
             h' <- inet_ntoa h
             return (fromIntegral p, B.pack $ map c2w h')
          x -> throwIO $ AddressNotSupportedException $ show x

    laddr <- getSocketName sock

    (lport,lhost) <-
        case laddr of
          SockAddrInet p h -> do
             h' <- inet_ntoa h
             return (fromIntegral p, B.pack $ map c2w h')
          x -> throwIO $ AddressNotSupportedException $ show x

    let c = Connection sock host port lhost lport

    forkOnIO cpu $ do
        labelMe $ "connHndl " ++ show fd
        bracket (return c)
                (\_ -> do
                     debug "sClose sock"
                     eatException $ shutdown sock ShutdownBoth
                     eatException $ sClose sock
                     eatException $ sClose sock
                )
                proc

    return ()


labelMe :: String -> IO ()
labelMe s = do
    tid <- myThreadId
    labelThread tid s


eatException :: IO a -> IO ()
eatException act = (act >> return ()) `catch` \(_::SomeException) -> return ()

getReadEnd :: Connection -> Enumerator IO a
getReadEnd = enumerate


getWriteEnd :: Connection -> Iteratee IO ()
getWriteEnd = writeOut


getRemoteAddr :: Connection -> ByteString
getRemoteAddr = _remoteAddr

getRemotePort :: Connection -> Int
getRemotePort = _remotePort

getLocalAddr :: Connection -> ByteString
getLocalAddr = _localAddr

getLocalPort :: Connection -> Int
getLocalPort = _localPort

------------------------------------------------------------------------------
getHostAddr :: Int
            -> ByteString
            -> IO SockAddr
getHostAddr p s = do
    h <- if s == "*"
          then return iNADDR_ANY
          else inet_addr (map w2c . B.unpack $ s)

    return $ SockAddrInet (fromIntegral p) h



data TimeoutException = TimeoutException
   deriving (Typeable)

instance Show TimeoutException where
    show TimeoutException = "timeout"

instance Exception TimeoutException


timeoutRecv :: Connection -> Int -> IO ByteString
timeoutRecv conn n = do
    let sock = _socket conn
    SB.recv sock n

timeoutSend :: Connection -> ByteString -> IO ()
timeoutSend conn s = do
    let sock = _socket conn
    SB.sendAll sock s


bLOCKSIZE :: Int
bLOCKSIZE = 8192


enumerate :: (MonadIO m) => Connection -> Enumerator m a
enumerate = loop
  where
    loop conn f = do
        es <- liftIO ((try $ timeoutRecv conn bLOCKSIZE)
                          :: IO (Either SomeException ByteString))

        case es of Left  e -> enumErr (show e) f
                   Right s -> sendOne conn f s

    sendOne conn f s = do
        v <- runIter f (if B.null s
                         then EOF Nothing
                         else Chunk $ WrapBS s)
        case v of
          r@(Done _ _)      -> return $ liftI r
          (Cont k Nothing)  -> loop conn k
          (Cont _ (Just e)) -> return $ throwErr e


writeOut :: (MonadIO m) => Connection -> Iteratee m ()
writeOut conn = IterateeG out
  where
    out c@(EOF _)   = return $ Done () c

    out (Chunk s) = do
        let x = unWrap s

        ee <- liftIO $ ((try $ timeoutSend conn x)
                            :: IO (Either SomeException ()))

        case ee of
          (Left e)  -> return $ Done () (EOF $ Just $ Err $ show e)
          (Right _) -> return $ Cont (writeOut conn) Nothing

