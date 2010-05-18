{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.LibevBackend
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

---------------------------
-- TODO: document module --
---------------------------

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString as B
import           Data.IORef
import           Data.Iteratee.WrappedByteString
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Foreign hiding (new)
import           Foreign.C.Error
import           Foreign.C.Types
import           GHC.Conc (forkOnIO)
import           Network.Libev
import           Network.Socket
import qualified Network.Socket.SendFile as SF
import           Prelude hiding (catch)
import           System.Timeout
------------------------------------------------------------------------------
import           Snap.Iteratee
import           Snap.Internal.Debug


data Backend = Backend
    { _acceptSocket      :: Socket
    , _acceptFd          :: CInt
    , _connectionQueue   :: Chan CInt
    , _evLoop            :: EvLoopPtr
    , _acceptIOCallback  :: FunPtr IoCallback
    , _acceptIOObj       :: EvIoPtr

      -- FIXME: we don't need _loopThread
    , _loopThread        :: MVar ThreadId
    , _mutexCallbacks    :: (FunPtr MutexCallback, FunPtr MutexCallback)
    , _loopLock          :: MVar ()
    , _asyncCb           :: FunPtr AsyncCallback
    , _asyncObj          :: EvAsyncPtr
    , _killCb           :: FunPtr AsyncCallback
    , _killObj          :: EvAsyncPtr
    , _connectionThreads :: MVar (Set ThreadId)
    , _backendCPU        :: Int
    }


data Connection = Connection
    { _backend        :: Backend
    , _socket         :: Socket
    , _socketFd       :: CInt
    , _remoteAddr     :: ByteString
    , _remotePort     :: Int
    , _localAddr      :: ByteString
    , _localPort      :: Int
    , _readAvailable  :: MVar ()
    , _writeAvailable :: MVar ()
    , _timerObj       :: EvTimerPtr
    , _timerCallback  :: FunPtr TimerCallback
    , _openingTime    :: CDouble
    , _lastActivity   :: IORef CDouble
    , _connIOObj      :: EvIoPtr
    , _connIOCallback :: FunPtr IoCallback
    , _connThread     :: MVar ThreadId
    }


sendFile :: Connection -> FilePath -> IO ()
sendFile c fp = do
    withMVar lock $ \_ -> evIoStop loop io
    SF.sendFile s fp
    withMVar lock $ \_ -> do
      tryPutMVar (_readAvailable c) ()
      tryPutMVar (_writeAvailable c) ()
      evIoStart loop io
      evAsyncSend loop asy

  where
    s    = _socket c
    io   = _connIOObj c
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
    listen sock bindPort
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

    -- an MVar for the loop thread, and one to keep track of the set of active
    -- threads
    threadMVar <- newEmptyMVar
    threadSetMVar <- newMVar Set.empty

    let b = Backend sock
                    sockFd
                    connq
                    lp
                    accCB
                    accIO
                    threadMVar
                    (mc1,mc2)
                    looplock
                    asyncCB
                    asyncObj
                    killCB
                    killObj
                    threadSetMVar
                    cpu

    tid <- forkOnIO cpu $ loopThread b
    putMVar threadMVar tid

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
    r <- c_accept accFd

    case r of
      -- this (EWOULDBLOCK) shouldn't happen (we just got told it was ready!),
      -- if it does (maybe the request got picked up by another thread) we'll
      -- just bail out
      -2 -> return ()
      -1 -> debugErrno "Backend.acceptCallback:c_accept()"
      fd -> writeChan chan fd


ioCallback :: MVar () -> MVar () -> IoCallback
ioCallback ra wa _loopPtr _ioPtr event = do
    -- send notifications to the worker thread
    when isRead  $ tryPutMVar ra () >> return ()
    when isWrite $ tryPutMVar wa () >> return ()

  where
    isRead  = (event .&. ev_read) /= 0
    isWrite = (event .&. ev_write) /= 0


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
    debug $ "Backend.stop: all threads dead, unlooping"

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
    tid <- readMVar tmv
    throwTo tid TimeoutException


freeConnection :: Connection -> IO ()
freeConnection conn = ignoreException $ do
    withMVar loopLock $ \_ -> block $ do
        -- close socket (twice to get proper linger behaviour)
        c_close fd
        c_close fd

        -- stop and free timer object
        evTimerStop loop timerObj
        freeEvTimer timerObj
        freeTimerCallback timerCb

        -- stop and free i/o object
        evIoStop loop ioObj
        freeEvIo ioObj
        freeIoCallback ioCb

        -- remove the thread id from the backend set
        tid <- readMVar threadMVar
        modifyMVar_ tsetMVar $ return . Set.delete tid

        -- wake up the event loop so it can be apprised of the changes
        evAsyncSend loop asyncObj

  where
    backend    = _backend conn
    tsetMVar   = _connectionThreads backend
    loop       = _evLoop backend
    loopLock   = _loopLock backend
    asyncObj   = _asyncObj backend

    fd         = _socketFd conn
    threadMVar = _connThread conn
    ioObj      = _connIOObj conn
    ioCb       = _connIOCallback conn
    timerObj   = _timerObj conn
    timerCb    = _timerCallback conn


ignoreException :: IO () -> IO ()
ignoreException = handle (\(_::SomeException) -> return ())


freeBackend :: Backend -> IO ()
freeBackend backend = ignoreException $ block $ do
    -- note: we only get here after an unloop

    withMVar tsetMVar $ \set -> do
        mapM_ killThread $ Set.toList set

    debug $ "Backend.freeBackend: wait at most 2 seconds for threads to die"
    waitForThreads backend $ seconds 2

    debug $ "Backend.freeBackend: all threads dead"

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
    tsetMVar    = _connectionThreads backend
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
        fd   <- readChan $ _connectionQueue backend

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

        now        <- evNow lp
        lastActRef <- newIORef now

        -- makes sense to assume the socket is read/write available when
        -- opened; worst-case is we get EWOULDBLOCK
        ra    <- newMVar ()
        wa    <- newMVar ()

        tmr   <- mkEvTimer
        thrmv <- newEmptyMVar
        tcb   <- mkTimerCallback $ timerCallback thrmv
        evTimerInit tmr tcb 20 0

        evio <- mkEvIo
        iocb <- mkIoCallback $ ioCallback ra wa
        evIoInit evio iocb fd (ev_read .|. ev_write)

        -- take ev_loop lock, start timer and io watchers
        withMVar (_loopLock backend) $ \_ -> do
             evTimerStart lp tmr
             evIoStart lp evio

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
                              now
                              lastActRef
                              evio
                              iocb
                              thrmv


        tid <- forkOnIO cpu $ threadProc conn

        modifyMVar_ (_connectionThreads backend) $ ins tid
        putMVar thrmv tid

      where
        ins !thr !s = let !r = Set.insert thr s in return (r `seq` r)


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


data TimeoutException = TimeoutException
   deriving (Typeable)

instance Show TimeoutException where
    show _ = "timeout"

instance Exception TimeoutException


recvData :: Connection -> Int -> IO ByteString
recvData conn n = do
    dbg "entered"
    allocaBytes n $ \cstr -> do
    sz <- throwErrnoIfMinus1RetryMayBlock
              "recvData"
              (c_read fd cstr (toEnum n))
              waitForLock

    dbg $ "sz returned " ++ show sz

    if sz == 0
      then return ""
      else B.packCStringLen ((castPtr cstr),(fromEnum sz))

  where
    dbg s = debug $ "Backend.recvData(" ++ show (_socketFd conn) ++ "): " ++ s

    fd          = _socketFd conn
    lock        = _readAvailable conn
    waitForLock = do
        dbg "waitForLock"
        takeMVar lock


sendData :: Connection -> ByteString -> IO ()
sendData conn bs = do
    let len = B.length bs
    dbg $ "entered w/ " ++ show len ++ " bytes"
    written <- B.unsafeUseAsCString bs $ \cstr ->
        throwErrnoIfMinus1RetryMayBlock
                   "sendData"
                   (c_write fd cstr (toEnum len))
                   waitForLock

    dbg $ "wrote " ++ show written ++ " bytes"

    let n = fromEnum written
    if n < len
       then sendData conn $ B.drop n bs
       else return ()

  where
    dbg s = debug $ "Backend.sendData(" ++ show (_socketFd conn) ++ "): " ++ s
    fd          = _socketFd conn
    lock        = _writeAvailable conn
    waitForLock = takeMVar lock


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

        ee <- liftIO $ ((try $ sendData conn x)
                            :: IO (Either SomeException ()))

        case ee of
          (Left e)  -> return $ Done () (EOF $ Just $ Err $ show e)
          (Right _) -> return $ Cont (writeOut conn) Nothing

