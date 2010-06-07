{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Http.Server.LibevBackend
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

---------------------------
-- TODO: document module --
---------------------------

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import "monads-fd" Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString as B
import           Data.DList (DList)
import qualified Data.DList as D
import           Data.IORef
import           Data.Iteratee.WrappedByteString
import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Foreign hiding (new)
import           Foreign.C.Error
import           Foreign.C.Types
import           GHC.Conc (forkOnIO)
import           Network.Libev
import           Network.Socket
import           Prelude hiding (catch)
import           System.Timeout
------------------------------------------------------------------------------
import           Snap.Iteratee
import           Snap.Internal.Debug

#if defined(HAS_SENDFILE)
import qualified System.SendFile as SF
import           System.Posix.IO
import           System.Posix.Types (Fd(..))
#endif

data Backend = Backend
    { _acceptSocket      :: !Socket
    , _acceptFd          :: !CInt
    , _connectionQueue   :: !(Chan CInt)
    , _evLoop            :: !EvLoopPtr
    , _acceptIOCallback  :: !(FunPtr IoCallback)
    , _acceptIOObj       :: !EvIoPtr
    , _mutexCallbacks    :: !(FunPtr MutexCallback, FunPtr MutexCallback)
    , _loopLock          :: !(MVar ())
    , _asyncCb           :: !(FunPtr AsyncCallback)
    , _asyncObj          :: !EvAsyncPtr
    , _killCb            :: !(FunPtr AsyncCallback)
    , _killObj           :: !EvAsyncPtr
    , _connectionThreads :: !(MVar (Set ThreadId))
    , _connThreadEdits   :: !(IORef (DList (Set ThreadId -> Set ThreadId)))
    , _connThreadId      :: !(MVar ThreadId)
    , _connThreadIsDone  :: !(MVar ())
    , _threadActivity    :: !(MVar ())
    , _backendCPU        :: !Int
    }


data Connection = Connection
    { _backend             :: !Backend
    , _socket              :: !Socket
    , _socketFd            :: !CInt
    , _remoteAddr          :: !ByteString
    , _remotePort          :: !Int
    , _localAddr           :: !ByteString
    , _localPort           :: !Int
    , _readAvailable       :: !(MVar ())
    , _writeAvailable      :: !(MVar ())
    , _timerObj            :: !EvTimerPtr
    , _timerCallback       :: !(FunPtr TimerCallback)
    , _readActive          :: !(IORef Bool)
    , _writeActive         :: !(IORef Bool)
    , _connReadIOObj       :: !EvIoPtr
    , _connReadIOCallback  :: !(FunPtr IoCallback)
    , _connWriteIOObj      :: !EvIoPtr
    , _connWriteIOCallback :: !(FunPtr IoCallback)
    , _connThread          :: !(MVar ThreadId)
    }

{-# INLINE name #-}
name :: ByteString
name = "libev"


sendFile :: Connection -> FilePath -> Int -> IO ()
#if defined(HAS_SENDFILE)
sendFile c fp sz = do
#else
sendFile c fp _ = do
#endif
    withMVar lock $ \_ -> do
      act <- readIORef $ _writeActive c
      when act $ evIoStop loop io
      writeIORef (_writeActive c) False
      evAsyncSend loop asy

#if defined(HAS_SENDFILE)
    fd <- openFd fp ReadOnly Nothing defaultFileFlags
    go fd 0 sz
#else
    -- no need to count bytes
    enumFile fp (getWriteEnd c) >>= run
    return ()
#endif

    withMVar lock $ \_ -> do
      tryTakeMVar $ _readAvailable c
      tryTakeMVar $ _writeAvailable c
      evAsyncSend loop asy

  where
#if defined(HAS_SENDFILE)
    go fd off bytes
      | bytes == 0 = return ()
      | otherwise  = do
            sent <- SF.sendFile sfd fd off bytes
            if sent < bytes
              then tickleTimeout c >> go fd (off+sent) (bytes-sent)
              else return ()

    sfd  = Fd $ _socketFd c
#endif
    io   = _connWriteIOObj c
    b    = _backend c
    loop = _evLoop b
    lock = _loopLock b
    asy  = _asyncObj b


bindIt :: ByteString         -- ^ bind address, or \"*\" for all
       -> Int                -- ^ port to bind to
       -> IO (Socket,CInt)
bindIt bindAddress bindPort = do
    sock <- socket AF_INET Stream 0
    addr <- getHostAddr bindPort bindAddress
    setSocketOption sock ReuseAddr 1
    bindSocket sock addr
    listen sock 150
    let sockFd = fdSocket sock
    c_setnonblocking sockFd
    return (sock, sockFd)


new :: (Socket,CInt)   -- ^ value you got from bindIt
    -> Int             -- ^ cpu
    -> IO Backend
new (sock,sockFd) cpu = do
    connq <- newChan

    -- We'll try kqueue on OSX even though the libev docs complain that it's
    -- "broken", in the hope that it works as expected for sockets
    f  <- evRecommendedBackends
    lp <- evLoopNew $ toEnum . fromEnum $ f .|. evbackend_kqueue


    -- we'll be working multithreaded so we need to set up locking for the C
    -- event loop struct
    (mc1,mc2,looplock) <- setupLockingForLoop lp

    -- setup async callbacks -- these allow us to wake up the main loop
    -- (normally blocked in c-land) from other threads
    asyncObj <- mkEvAsync
    asyncCB  <- mkAsyncCallback $ \_ _ _ -> do
                            debug "async wakeup"
                            return ()

    killObj <- mkEvAsync
    killCB  <- mkAsyncCallback $ \_ _ _ -> do
                            debug "async kill wakeup"
                            evUnloop lp 2
                            return ()

    evAsyncInit asyncObj asyncCB
    evAsyncStart lp asyncObj
    evAsyncInit killObj killCB
    evAsyncStart lp killObj

    -- setup the accept callback; this watches for read readiness on the listen
    -- port
    accCB <- mkIoCallback $ acceptCallback sockFd connq
    accIO <- mkEvIo
    evIoInit accIO accCB sockFd ev_read
    evIoStart lp accIO

    -- thread set stuff
    connThreadMVar <- newEmptyMVar
    connSet        <- newMVar Set.empty
    editsRef       <- newIORef D.empty
    connThreadDone <- newEmptyMVar
    threadActivity <- newMVar ()

    let b = Backend sock
                    sockFd
                    connq
                    lp
                    accCB
                    accIO
                    (mc1,mc2)
                    looplock
                    asyncCB
                    asyncObj
                    killCB
                    killObj
                    connSet
                    editsRef
                    connThreadMVar
                    connThreadDone
                    threadActivity
                    cpu

    forkOnIO cpu $ loopThread b

    conntid <- forkOnIO cpu $ connTableSeqThread b
    putMVar connThreadMVar conntid

    debug $ "Backend.new: loop spawned"
    return b


-- | Run evLoop in a thread
loopThread :: Backend -> IO ()
loopThread backend = do
    debug $ "starting loop"
    (ignoreException go) `finally` cleanup
    debug $ "loop finished"
  where
    cleanup = do
        debug $ "loopThread: cleaning up"
        ignoreException $ freeBackend backend
    lock    = _loopLock backend
    loop    = _evLoop backend
    go      = takeMVar lock >> block (evLoop loop 0)


acceptCallback :: CInt -> Chan CInt -> IoCallback
acceptCallback accFd chan _loopPtr _ioPtr _ = do
    debug "inside acceptCallback"
    r <- c_accept accFd

    case r of
      -- this (EWOULDBLOCK) shouldn't happen (we just got told it was ready!),
      -- if it does (maybe the request got picked up by another thread) we'll
      -- just bail out
      -2 -> return ()
      -1 -> debugErrno "Backend.acceptCallback:c_accept()"
      fd -> do
          debug $ "acceptCallback: accept()ed fd, writing to chan " ++ show fd
          writeChan chan fd


ioReadCallback :: CInt -> IORef Bool -> MVar () -> IoCallback
ioReadCallback fd active ra _loopPtr _ioPtr _ = do
    -- send notifications to the worker thread
    debug $ "ioReadCallback: notification (" ++ show fd ++ ")"
    tryPutMVar ra ()
    debug $ "stopping ioReadCallback (" ++ show fd ++ ")"
    evIoStop _loopPtr _ioPtr
    writeIORef active False


ioWriteCallback :: CInt -> IORef Bool -> MVar () -> IoCallback
ioWriteCallback fd active wa _loopPtr _ioPtr _ = do
    -- send notifications to the worker thread
    debug $ "ioWriteCallback: notification (" ++ show fd ++ ")"
    tryPutMVar wa ()
    debug $ "stopping ioWriteCallback (" ++ show fd ++ ")"
    evIoStop _loopPtr _ioPtr
    writeIORef active False


seconds :: Int -> Int
seconds n = n * ((10::Int)^(6::Int))


stop :: Backend -> IO ()
stop b = ignoreException $ do
    debug $ "Backend.stop"

    -- FIXME: what are we gonna do here?
    --
    -- 1. take the loop lock
    -- 2. shut down the accept() callback
    -- 3. stuff a poison pill (a bunch of -1 values should do) down the
    --    connection queue so that withConnection knows to throw an exception
    --    back up to its caller
    -- 4. release the loop lock
    -- 5. wait until all of the threads have finished, or until 10 seconds have
    --    elapsed, whichever comes first
    -- 6. take the loop lock
    -- 7. call evUnloop and wake up the loop using evAsyncSend
    -- 8. release the loop lock, the main loop thread should then free/clean
    --    everything up (threads, connections, io objects, callbacks, etc)

    withMVar lock $ \_ -> do
        evIoStop loop acceptObj
        replicateM_ 10 $ writeChan connQ (-1)

    debug $ "Backend.stop: waiting at most 10 seconds for connection threads to die"
    waitForThreads b $ seconds 10
    debug $ "Backend.stop: all threads presumed dead, unlooping"

    withMVar lock $ \_ -> do
        -- FIXME: hlibev should export EVUNLOOP_ALL
        evUnloop loop 2
        evAsyncSend loop killObj

    debug $ "unloop sent"


  where
    loop           = _evLoop b
    acceptObj      = _acceptIOObj b
    killObj        = _killObj b
    lock           = _loopLock b
    connQ          = _connectionQueue b



waitForThreads :: Backend -> Int -> IO ()
waitForThreads backend t = timeout t wait >> return ()
  where
    threadSet = _connectionThreads backend
    wait = do
        threads <- readMVar threadSet
        if (Set.null threads)
          then return ()
          else threadDelay (seconds 1) >> wait



getAddr :: SockAddr -> IO (ByteString, Int)
getAddr addr =
    case addr of
      SockAddrInet p ha -> do
          s <- liftM (B.pack . map c2w) (inet_ntoa ha)
          return (s, fromIntegral p)

      a -> throwIO $ AddressNotSupportedException (show a)


-- | throw a timeout exception to the handling thread -- it'll clean up
-- everything
timerCallback :: MVar ThreadId -> TimerCallback
timerCallback tmv _ _ _ = do
    debug "Backend.timerCallback: timed out"
    tid <- readMVar tmv
    throwTo tid TimeoutException


addThreadSetEdit :: Backend -> (Set ThreadId -> Set ThreadId) -> IO ()
addThreadSetEdit backend edit = do
    atomicModifyIORef (_connThreadEdits backend) $ \els ->
        (D.snoc els edit, ())

    tryPutMVar (_threadActivity backend) ()
    return ()


freeConnection :: Connection -> IO ()
freeConnection conn = ignoreException $ do
    withMVar loopLock $ \_ -> block $ do
        debug $ "freeConnection (" ++ show fd ++ ")"

        c_close fd

        -- stop and free timer object
        evTimerStop loop timerObj
        freeEvTimer timerObj
        freeTimerCallback timerCb

        -- stop and free i/o objects
        evIoStop loop ioWrObj
        freeEvIo ioWrObj
        freeIoCallback ioWrCb

        evIoStop loop ioRdObj
        freeEvIo ioRdObj
        freeIoCallback ioRdCb

        tid <- readMVar $ _connThread conn

        -- schedule the removal of the thread id from the backend set
        addThreadSetEdit backend (Set.delete tid)

        -- wake up the event loop so it can be apprised of the changes
        evAsyncSend loop asyncObj

  where
    backend    = _backend conn
    loop       = _evLoop backend
    loopLock   = _loopLock backend
    asyncObj   = _asyncObj backend

    fd         = _socketFd conn
    ioWrObj    = _connWriteIOObj conn
    ioWrCb     = _connWriteIOCallback conn
    ioRdObj    = _connReadIOObj conn
    ioRdCb     = _connReadIOCallback conn
    timerObj   = _timerObj conn
    timerCb    = _timerCallback conn


ignoreException :: IO () -> IO ()
ignoreException = handle (\(_::SomeException) -> return ())


connTableSeqThread :: Backend -> IO ()
connTableSeqThread backend = loop `finally` putMVar threadDone ()
  where
    threadDone = _connThreadIsDone backend
    editsRef   = _connThreadEdits backend
    table      = _connectionThreads backend
    activity   = _threadActivity backend

    loop = do
        takeMVar activity

        -- grab the edits
        edits <- atomicModifyIORef editsRef $ \t -> (D.empty, D.toList t)

        -- apply the edits
        modifyMVar_ table $ \t -> block $ do
               let !t' = List.foldl' (flip ($)) t edits
               return t'

        -- zzz
        threadDelay 1000000
        loop


freeBackend :: Backend -> IO ()
freeBackend backend = ignoreException $ block $ do
    -- note: we only get here after an unloop

    readMVar (_connThreadId backend) >>= killThread
    takeMVar $ _connThreadIsDone backend

    -- read edits and obtain final thread table
    threads <- withMVar (_connectionThreads backend) $ \table -> do
                   edits <- liftM D.toList $
                            readIORef (_connThreadEdits backend)

                   let !t = List.foldl' (flip ($)) table edits
                   return $ Set.toList t

    mapM_ killThread threads

    debug $ "Backend.freeBackend: all threads killed"
    debug $ "Backend.freeBackend: destroying resources"
    freeEvIo acceptObj
    freeIoCallback acceptCb
    c_close fd

    evAsyncStop loop asyncObj
    freeEvAsync asyncObj
    freeAsyncCallback asyncCb

    evAsyncStop loop killObj
    freeEvAsync killObj
    freeAsyncCallback killCb

    freeMutexCallback mcb1
    freeMutexCallback mcb2

    evLoopDestroy loop
    debug $ "Backend.freeBackend: resources destroyed"

  where
    fd          = _acceptFd backend
    acceptObj   = _acceptIOObj backend
    acceptCb    = _acceptIOCallback backend
    asyncObj    = _asyncObj backend
    asyncCb     = _asyncCb backend
    killObj     = _killObj backend
    killCb      = _killCb backend
    (mcb1,mcb2) = _mutexCallbacks backend
    loop        = _evLoop backend


-- | Note: proc gets run in the background
withConnection :: Backend -> Int -> (Connection -> IO ()) -> IO ()
withConnection backend cpu proc = go
  where
    threadProc conn = ignoreException (proc conn) `finally` freeConnection conn

    go = do
        debug $ "withConnection: reading from chan"
        fd   <- readChan $ _connectionQueue backend
        debug $ "withConnection: got fd " ++ show fd

        -- if fd < 0 throw an exception here (because this only happens if stop
        -- is called)
        when (fd < 0) $ throwIO BackendTerminatedException

        sock <- mkSocket fd AF_INET Stream 0 Connected
        peerName <- getPeerName sock
        sockName <- getSocketName sock

        -- set_linger fd
        c_setnonblocking fd

        (remoteAddr, remotePort) <- getAddr peerName
        (localAddr, localPort) <- getAddr sockName

        let lp = _evLoop backend

        -- makes sense to assume the socket is read/write available when
        -- opened; worst-case is we get EWOULDBLOCK
        ra    <- newMVar ()
        wa    <- newMVar ()

        tmr   <- mkEvTimer
        thrmv <- newEmptyMVar
        tcb   <- mkTimerCallback $ timerCallback thrmv
        evTimerInit tmr tcb 0 20.0

        readActive  <- newIORef True
        writeActive <- newIORef True

        evioRead <- mkEvIo
        ioReadCb <- mkIoCallback $ ioReadCallback fd readActive ra

        evioWrite <- mkEvIo
        ioWriteCb <- mkIoCallback $ ioWriteCallback fd writeActive wa

        evIoInit evioRead ioReadCb fd ev_read
        evIoInit evioWrite ioWriteCb fd ev_write

        -- take ev_loop lock, start timer and io watchers
        withMVar (_loopLock backend) $ \_ -> do
             evTimerAgain lp tmr
             evIoStart lp evioRead
             evIoStart lp evioWrite

             -- wakeup the loop thread so that these new watchers get
             -- registered next time through the loop
             evAsyncSend lp $ _asyncObj backend

        let conn = Connection backend
                              sock
                              fd
                              remoteAddr
                              remotePort
                              localAddr
                              localPort
                              ra
                              wa
                              tmr
                              tcb
                              readActive
                              writeActive
                              evioRead
                              ioReadCb
                              evioWrite
                              ioWriteCb
                              thrmv


        tid <- forkOnIO cpu $ threadProc conn

        addThreadSetEdit backend (Set.insert tid)
        putMVar thrmv tid


data BackendTerminatedException = BackendTerminatedException
   deriving (Typeable)

instance Show BackendTerminatedException where
    show BackendTerminatedException = "Backend terminated"

instance Exception BackendTerminatedException



data AddressNotSupportedException = AddressNotSupportedException String
   deriving (Typeable)

instance Show AddressNotSupportedException where
    show (AddressNotSupportedException x) = "Address not supported: " ++ x

instance Exception AddressNotSupportedException


getRemoteAddr :: Connection -> ByteString
getRemoteAddr = _remoteAddr

getRemotePort :: Connection -> Int
getRemotePort = _remotePort

getLocalAddr :: Connection -> ByteString
getLocalAddr = _localAddr

getLocalPort :: Connection -> Int
getLocalPort = _localPort

------------------------------------------------------------------------------

-- fixme: new function name
getHostAddr :: Int
            -> ByteString
            -> IO SockAddr
getHostAddr p s = do
    h <- if s == "*"
          then return iNADDR_ANY
          else inet_addr (map w2c . B.unpack $ s)

    return $ SockAddrInet (fromIntegral p) h



bLOCKSIZE :: Int
bLOCKSIZE = 8192

--
-- About timeouts
--
-- It's not good enough to restart the timer from io(Read|Write)Callback,
-- because those seem to be edge-triggered. I've definitely had where after
-- 20 seconds they still weren't being re-awakened.
--

data TimeoutException = TimeoutException
   deriving (Typeable)

instance Show TimeoutException where
    show _ = "timeout"

instance Exception TimeoutException

tickleTimeout :: Connection -> IO ()
tickleTimeout conn = do
    debug "Backend.tickleTimeout"
    withMVar (_loopLock bk) $ \_ -> evTimerAgain lp tmr

  where
    bk  = _backend conn
    lp  = _evLoop bk
    tmr = _timerObj conn

recvData :: Connection -> Int -> IO ByteString
recvData conn n = do
    dbg "entered"
    allocaBytes n $ \cstr -> do
    sz <- throwErrnoIfMinus1RetryMayBlock
              "recvData"
              (c_read fd cstr (toEnum n))
              waitForLock

    -- we got activity, but don't do restart timer due to the
    -- slowloris attack

    dbg $ "sz returned " ++ show sz

    if sz == 0
      then return ""
      else B.packCStringLen ((castPtr cstr),(fromEnum sz))

  where
    io       = _connReadIOObj conn
    bk       = _backend conn
    active   = _readActive conn
    lp       = _evLoop bk
    looplock = _loopLock bk
    async    = _asyncObj bk

    dbg s = debug $ "Backend.recvData(" ++ show (_socketFd conn) ++ "): " ++ s

    fd          = _socketFd conn
    lock        = _readAvailable conn
    waitForLock = do
        dbg "start waitForLock"

        withMVar looplock $ \_ -> do
            act <- readIORef active
            if act
              then dbg "read watcher already active, skipping"
              else do
                dbg "starting watcher, sending async"
                tryTakeMVar lock
                evIoStart lp io
                writeIORef active True
                evAsyncSend lp async

        dbg "waitForLock: waiting for mvar"
        takeMVar lock
        dbg "waitForLock: took mvar"


sendData :: Connection -> ByteString -> IO ()
sendData conn bs = do
    let len = B.length bs
    dbg $ "entered w/ " ++ show len ++ " bytes"
    written <- B.unsafeUseAsCString bs $ \cstr ->
        throwErrnoIfMinus1RetryMayBlock
                   "sendData"
                   (c_write fd cstr (toEnum len))
                   waitForLock

    -- we got activity, so restart timer
    tickleTimeout conn

    let n = fromEnum written
    let last10 = B.drop (n-10) $ B.take n bs

    dbg $ "wrote " ++ show written ++ " bytes, last 10='" ++ show last10 ++ "'"

    if n < len
       then do
         dbg $ "short write, need to write " ++ show (len-n) ++ " more bytes"
         sendData conn $ B.drop n bs
       else return ()

  where
    io       = _connWriteIOObj conn
    bk       = _backend conn
    active   = _writeActive conn
    lp       = _evLoop bk
    looplock = _loopLock bk
    async    = _asyncObj bk

    dbg s = debug $ "Backend.sendData(" ++ show (_socketFd conn) ++ "): " ++ s
    fd          = _socketFd conn
    lock        = _writeAvailable conn
    waitForLock = do
        dbg "waitForLock: starting"
        withMVar looplock $ \_ -> do
            act <- readIORef active
            if act
              then dbg "write watcher already running, skipping"
              else do
                dbg "starting watcher, sending async event"
                tryTakeMVar lock
                evIoStart lp io
                writeIORef active True
                evAsyncSend lp async

        dbg "waitForLock: taking mvar"
        takeMVar lock
        dbg "waitForLock: took mvar"


getReadEnd :: Connection -> Enumerator IO a
getReadEnd = enumerate


getWriteEnd :: Connection -> Iteratee IO ()
getWriteEnd = writeOut


enumerate :: (MonadIO m) => Connection -> Enumerator m a
enumerate = loop
  where
    loop conn f = do
        s <- liftIO $ recvData conn bLOCKSIZE
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

        liftIO $ sendData conn x

        return $ Cont (writeOut conn) Nothing

