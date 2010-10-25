{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Snap.Internal.Http.Server.SimpleBackend
  ( Backend
  , BackendTerminatedException(..)
  , Connection
  , TimeoutException(..)
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
import "monads-fd" Control.Monad.Trans

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString as B
import           Data.Iteratee.WrappedByteString
import           Data.Typeable
import           Data.Word
import           Foreign hiding (new)
import           GHC.Conc (labelThread, forkOnIO)
import           Network.Socket
import qualified Network.Socket.ByteString as SB
import           Prelude hiding (catch)
------------------------------------------------------------------------------
import           Data.Concurrent.HashMap (hashString)
import           Snap.Internal.Debug
import           Snap.Internal.Http.Server.Date
import qualified Snap.Internal.Http.Server.TimeoutTable as TT
import           Snap.Internal.Http.Server.TimeoutTable (TimeoutTable)
import           Snap.Iteratee hiding (foldl')

#if defined(HAS_SENDFILE)
import qualified System.SendFile as SF
import           System.Posix.IO
import           System.Posix.Types (Fd(..))
#endif


data BackendTerminatedException = BackendTerminatedException
   deriving (Typeable)

instance Show BackendTerminatedException where
    show (BackendTerminatedException) = "Backend terminated"

instance Exception BackendTerminatedException


------------------------------------------------------------------------------
type QueueElem = Maybe (Socket,SockAddr)

data Backend = Backend
    { _acceptSocket    :: !Socket
    , _acceptThread    :: !ThreadId
    , _timeoutTable    :: TimeoutTable
    , _timeoutThread   :: !(MVar ThreadId)
    , _connectionQueue :: !(Chan QueueElem)
    }

data Connection = Connection
    { _backend     :: Backend
    , _socket      :: Socket
    , _remoteAddr  :: ByteString
    , _remotePort  :: Int
    , _localAddr   :: ByteString
    , _localPort   :: Int
    , _connTid     :: MVar ThreadId
    , _threadHash  :: MVar Word
    }


{-# INLINE name #-}
name :: ByteString
name = "simple"


sendFile :: Connection -> FilePath -> Int64 -> Int64 -> IO ()
#if defined(HAS_SENDFILE)
sendFile c fp start sz = do
    bracket (openFd fp ReadOnly Nothing defaultFileFlags)
            (closeFd)
            (go start sz)
  where
    go off bytes fd
      | bytes == 0 = return ()
      | otherwise  = do
            sent <- SF.sendFile sfd fd off bytes
            if sent < bytes
              then tickleTimeout c >> go (off+sent) (bytes-sent) fd
              else return ()

    sfd = Fd . fdSocket $ _socket c
#else
sendFile c fp start sz = do
    -- no need to count bytes
    enumFilePartial fp (start,start+sz) (getWriteEnd c) >>= run
    return ()
#endif


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


acceptThread :: Socket -> Chan QueueElem -> IO ()
acceptThread sock connq = loop `finally` cleanup
  where
    loop = do
        debug $ "acceptThread: calling accept()"
        s@(_,addr) <- accept sock
        debug $ "acceptThread: accepted connection from remote: " ++ show addr
        debug $ "acceptThread: queueing"
        writeChan connq $ Just s
        loop

    cleanup = block $ do
        debug $ "acceptThread: cleanup, closing socket and notifying "
                  ++ "chan listeners"
        sClose sock
        replicateM 10 $ writeChan connq Nothing


new :: Socket   -- ^ value you got from bindIt
    -> Int
    -> IO Backend
new sock cpu = do
    debug $ "Backend.new: listening"

    tt        <- TT.new
    t         <- newEmptyMVar
    connq     <- newChan
    accThread <- forkOnIO cpu $ acceptThread sock connq

    let b = Backend sock accThread tt t connq

    tid <- forkIO $ timeoutThread b
    putMVar t tid

    return b


timeoutThread :: Backend -> IO ()
timeoutThread backend = do
    loop `catch` (\(_::SomeException) -> killAll)

  where
    table = _timeoutTable backend

    loop = do
        debug "timeoutThread: waiting for activity on thread table"
        TT.waitForActivity table
        debug "timeoutThread: woke up, killing old connections"
        killTooOld
        loop


    killTooOld = do
        now    <- getCurrentDateTime
        TT.killOlderThan (now - tIMEOUT) table

    -- timeout = 30 seconds
    tIMEOUT = 30

    killAll = do
        debug "Backend.timeoutThread: shutdown, killing all connections"
        TT.killAll table


stop :: Backend -> IO ()
stop backend = do
    debug $ "Backend.stop: killing accept thread"
    killThread acthr

    debug $ "Backend.stop: killing timeout thread"

    -- kill timeout thread; timeout thread handler will stop all of the running
    -- connection threads
    readMVar tthr >>= killThread
    debug $ "Backend.stop: exiting.."

  where
    acthr = _acceptThread  backend
    tthr  = _timeoutThread backend


data AddressNotSupportedException = AddressNotSupportedException String
   deriving (Typeable)

instance Show AddressNotSupportedException where
    show (AddressNotSupportedException x) = "Address not supported: " ++ x

instance Exception AddressNotSupportedException


withConnection :: Backend -> Int -> (Connection -> IO ()) -> IO ()
withConnection backend cpu proc = do
    debug $ "Backend.withConnection: reading from chan"

    qelem <- readChan $ _connectionQueue backend
    when (qelem == Nothing) $ do
        debug $ "Backend.withConnection: channel terminated, throwing "
                  ++ "BackendTerminatedException"
        throwIO BackendTerminatedException

    let (Just (sock,addr)) = qelem
    let fd = fdSocket sock

    debug $ "Backend.withConnection: dequeued connection from remote: "
              ++ show addr

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

    tmvar   <- newEmptyMVar
    thrhash <- newEmptyMVar

    let c = Connection backend sock host port lhost lport tmvar thrhash

    tid <- forkOnIO cpu $ do
        labelMe $ "connHndl " ++ show fd
        bracket (return c)
                (\_ -> block $ do
                     debug "thread killed, closing socket"
                     thr   <- readMVar tmvar
                     thash <- readMVar thrhash

                     -- remove thread from timeout table
                     TT.delete thash thr $ _timeoutTable backend

                     eatException $ shutdown sock ShutdownBoth
                     eatException $ sClose sock
                )
                proc

    putMVar tmvar tid
    putMVar thrhash $ hashString $ show tid
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
tickleTimeout conn = do
    debug "Backend.tickleTimeout"
    now   <- getCurrentDateTime
    tid   <- readMVar $ _connTid conn
    thash <- readMVar $ _threadHash conn

    TT.insert thash tid now table

  where
    table = _timeoutTable $ _backend conn


_cancelTimeout :: Connection -> IO ()
_cancelTimeout conn = do
    debug "Backend.cancelTimeout"

    tid   <- readMVar $ _connTid conn
    thash <- readMVar $ _threadHash conn

    TT.delete thash tid table

  where
    table = _timeoutTable $ _backend conn


timeoutRecv :: Connection -> Int -> IO ByteString
timeoutRecv conn n = do
    let sock = _socket conn
    SB.recv sock n


timeoutSend :: Connection -> ByteString -> IO ()
timeoutSend conn s = do
    let len = B.length s
    debug $ "Backend.timeoutSend: entered w/ " ++ show len ++ " bytes"
    let sock = _socket conn
    SB.sendAll sock s
    debug $ "Backend.timeoutSend: sent all"
    tickleTimeout conn


bLOCKSIZE :: Int
bLOCKSIZE = 8192


enumerate :: (MonadIO m) => Connection -> Enumerator m a
enumerate = loop
  where
    loop conn f = do
        debug $ "Backend.enumerate: reading from socket"
        s <- liftIO $ timeoutRecv conn bLOCKSIZE
        debug $ "Backend.enumerate: got " ++ Prelude.show (B.length s)
                ++ " bytes from read end"
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

