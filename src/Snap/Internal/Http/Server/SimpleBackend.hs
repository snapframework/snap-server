{-# LANGUAGE BangPatterns #-}
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
  , name
  , debug
  , bindIt
  , new
  , stop
  , withConnection
  , sendFile
  , tickleTimeout
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
import qualified Data.PSQueue as PSQ
import           Data.PSQueue (PSQ)
import           Data.Typeable
import           Foreign hiding (new)
import           Foreign.C.Types (CTime)
import           GHC.Conc (labelThread, forkOnIO)
import           Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.Socket.SendFile as SF
import           Prelude hiding (catch)
------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Internal.Http.Server.Date
import           Snap.Iteratee


data BackendTerminatedException = BackendTerminatedException
   deriving (Typeable)

instance Show BackendTerminatedException where
    show (BackendTerminatedException) = "Backend terminated"

instance Exception BackendTerminatedException

data Backend = Backend
    { _acceptSocket  :: Socket
    , _timeoutTable  :: MVar (PSQ ThreadId CTime)
    , _timeoutThread :: MVar ThreadId }

data Connection = Connection 
    { _backend     :: Backend
    , _socket      :: Socket
    , _remoteAddr  :: ByteString
    , _remotePort  :: Int
    , _localAddr   :: ByteString
    , _localPort   :: Int
    , _connTid     :: MVar ThreadId }

{-# INLINE name #-}
name :: ByteString
name = "simple"


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
    listen sock 150
    return sock


new :: Socket   -- ^ value you got from bindIt
    -> Int
    -> IO Backend
new sock _ = do
    debug $ "Backend.new: listening"

    mv  <- newMVar PSQ.empty
    t   <- newEmptyMVar

    let b = Backend sock mv t

    tid <- forkIO $ timeoutThread b
    putMVar t tid

    return b


timeoutThread :: Backend -> IO ()
timeoutThread backend = loop
  where
    loop = do
        killTooOld
        threadDelay (5000000)
        loop


    killTooOld = modifyMVar_ tmvar $ \table -> do
        now <- getCurrentDateTime
        !t' <- killOlderThan now table
        return t'


    -- timeout = 60 seconds
    tIMEOUT = 60

    killOlderThan now !table = do
        let mmin = PSQ.findMin table
        maybe (return table)
              (\m -> if now - PSQ.prio m > tIMEOUT
                       then do
                           killThread $ PSQ.key m
                           killOlderThan now $ PSQ.deleteMin table
                       else return table)
              mmin

    tmvar = _timeoutTable backend


stop :: Backend -> IO ()
stop (Backend s _ t) = do
    debug $ "Backend.stop"
    sClose s

    -- kill timeout thread and current thread
    readMVar t >>= killThread
    myThreadId >>= killThread


data AddressNotSupportedException = AddressNotSupportedException String
   deriving (Typeable)

instance Show AddressNotSupportedException where
    show (AddressNotSupportedException x) = "Address not supported: " ++ x

instance Exception AddressNotSupportedException


withConnection :: Backend -> Int -> (Connection -> IO ()) -> IO ()
withConnection backend cpu proc = do
    debug $ "Backend.withConnection: calling accept()"
    let asock = _acceptSocket backend
    (sock,addr) <- accept asock

    let fd = fdSocket sock

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

    tmvar <- newEmptyMVar

    let c = Connection backend sock host port lhost lport tmvar

    tid <- forkOnIO cpu $ do
        labelMe $ "connHndl " ++ show fd
        bracket (return c)
                (\_ -> block $ do
                     debug "sClose sock"
                     thr <- readMVar tmvar

                     -- remove thread from timeout table
                     modifyMVar_ (_timeoutTable backend) $
                                 return . PSQ.delete thr
                     eatException $ shutdown sock ShutdownBoth
                     eatException $ sClose sock
                )
                proc

    putMVar tmvar tid
    tickleTimeout c
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


tickleTimeout :: Connection -> IO ()
tickleTimeout conn = modifyMVar_ ttmvar $ \t -> do
    now <- getCurrentDateTime
    tid <- readMVar $ _connTid conn
    let !t' = PSQ.insert tid now t
    return t'

  where
    ttmvar = _timeoutTable $ _backend conn


timeoutRecv :: Connection -> Int -> IO ByteString
timeoutRecv conn n = do
    let sock = _socket conn
    SB.recv sock n


timeoutSend :: Connection -> ByteString -> IO ()
timeoutSend conn s = do
    let sock = _socket conn
    SB.sendAll sock s
    tickleTimeout conn


bLOCKSIZE :: Int
bLOCKSIZE = 8192


enumerate :: (MonadIO m) => Connection -> Enumerator m a
enumerate = loop
  where
    loop conn f = do
        s <- liftIO $ timeoutRecv conn bLOCKSIZE
        sendOne conn f s

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

