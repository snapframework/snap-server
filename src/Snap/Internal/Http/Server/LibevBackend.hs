{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Http.Server.LibevBackend
  ( libEvEventLoop
  ) where

#ifndef LIBEV

import Control.Exception
import Data.Typeable
import Snap.Internal.Http.Server.Backend

data LibevException = LibevException String
  deriving (Show, Typeable)
instance Exception LibevException

libEvEventLoop :: EventLoop
libEvEventLoop _ _ _ _ = throwIO $ LibevException "libev event loop is not supported"

#else

---------------------------
-- TODO: document module --
---------------------------

------------------------------------------------------------------------------
import             Control.Concurrent hiding (yield)
import             Control.Exception
import             Control.Monad
import             Control.Monad.Trans
import             Data.ByteString (ByteString)
import             Data.ByteString.Internal (c2w)
import qualified   Data.ByteString as S
import             Data.Maybe
import             Data.IORef
import             Data.Typeable
import             Foreign hiding (new)
import             Foreign.C.Types
import             GHC.Conc (forkOnIO)
import             Network.Libev
import             Network.Socket
import             Prelude hiding (catch)
------------------------------------------------------------------------------

-- FIXME: should be HashSet, make that later.
import qualified   Data.Concurrent.HashMap as H
import             Data.Concurrent.HashMap (HashMap)
import             Snap.Iteratee hiding (map)
import             Snap.Internal.Debug
import             Snap.Internal.Http.Server.Date
import             Snap.Internal.Http.Server.Backend
import qualified   Snap.Internal.Http.Server.ListenHelpers as Listen

#if defined(HAS_SENDFILE)
import qualified   System.SendFile as SF
import             System.Posix.IO
import             System.Posix.Types (Fd(..))
#endif

data Backend = Backend
    { _acceptSockets     :: [ListenSocket]
    , _evLoop            :: !EvLoopPtr
    , _acceptIOCallbacks :: ![MVar (FunPtr IoCallback)]
    , _acceptIOObjs      :: ![EvIoPtr]
    , _mutexCallbacks    :: !(FunPtr MutexCallback, FunPtr MutexCallback)
    , _loopLock          :: !(MVar ())
    , _asyncCb           :: !(FunPtr AsyncCallback)
    , _asyncObj          :: !EvAsyncPtr
    , _killCb            :: !(FunPtr AsyncCallback)
    , _killObj           :: !EvAsyncPtr
    , _connectionThreads :: !(HashMap ThreadId Connection)
    , _backendCPU        :: !Int
    , _loopExit          :: !(MVar ())
    }


data Connection = Connection
    { _backend             :: !Backend
    , _listenSocket        :: !ListenSocket
    , _rawSocket           :: !CInt
    , _sessionInfo         :: !SessionInfo
    , _readAvailable       :: !(MVar ())
    , _writeAvailable      :: !(MVar ())
    , _timerObj            :: !EvTimerPtr
    , _timerCallback       :: !(FunPtr TimerCallback)
    , _timerTimeoutTime    :: !(IORef CTime)
    , _readActive          :: !(IORef Bool)
    , _writeActive         :: !(IORef Bool)
    , _connReadIOObj       :: !EvIoPtr
    , _connReadIOCallback  :: !(FunPtr IoCallback)
    , _connWriteIOObj      :: !EvIoPtr
    , _connWriteIOCallback :: !(FunPtr IoCallback)
    , _connThread          :: !(ThreadId)
    }

libEvEventLoop :: EventLoop
libEvEventLoop sockets cap elog handler = do
    backends <- Prelude.mapM (newLoop sockets handler elog) [0..(cap-1)]

    debug "libevEventLoop: waiting for loop exit"
    Prelude.mapM_ (takeMVar . _loopExit) backends `finally` do
        debug "libevEventLoop: stopping all backends"
        mapM stop backends
        mapM Listen.closeSocket sockets

newLoop :: [ListenSocket]        -- ^ value you got from bindIt
        -> SessionHandler        -- ^ handler
        -> (ByteString -> IO ()) -- ^ error logger
        -> Int                   -- ^ cpu
        -> IO Backend
newLoop sockets handler elog cpu = do
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
                            evUnloop lp evunloop_all
                            return ()

    evAsyncInit asyncObj asyncCB
    evAsyncStart lp asyncObj
    evAsyncInit killObj killCB
    evAsyncStart lp killObj

    -- create the ios for the accept callbacks
    accMVars <- forM sockets $ \_ -> newEmptyMVar
    accIOs <- forM sockets $ \_ -> mkEvIo

    -- thread set stuff
    connSet <- H.new (H.hashString . show)

    -- freed gets stuffed with () when all resources are released.
    freed <- newEmptyMVar

    let b = Backend sockets
                    lp
                    accMVars
                    accIOs
                    (mc1,mc2)
                    looplock
                    asyncCB
                    asyncObj
                    killCB
                    killObj
                    connSet
                    cpu
                    freed

    -- setup the accept callback; this watches for read readiness on the listen
    -- port
    forM_ (zip3 sockets accIOs accMVars) $ \(sock, accIO, x) -> do
        accCB <- mkIoCallback $ acceptCallback b handler elog cpu sock
        evIoInit accIO accCB (fdSocket $ Listen.listenSocket sock) ev_read
        evIoStart lp accIO
        putMVar x accCB

    forkOnIO cpu $ loopThread b

    debug $ "Backend.newLoop: loop spawned"
    return b


-- | Run evLoop in a thread
loopThread :: Backend -> IO ()
loopThread backend = do
    debug $ "starting loop"
    (ignoreException go) `finally` cleanup
    debug $ "loop finished"
  where
    cleanup = block $ do
        debug $ "loopThread: cleaning up"
        ignoreException $ freeBackend backend
        putMVar (_loopExit backend) ()

    lock    = _loopLock backend
    loop    = _evLoop backend
    go      = takeMVar lock >> block (evLoop loop 0)


acceptCallback :: Backend
               -> SessionHandler 
               -> (ByteString -> IO ())
               -> Int
               -> ListenSocket 
               -> IoCallback
acceptCallback back handler elog cpu sock _loopPtr _ioPtr _ = do
    debug "inside acceptCallback"
    r <- c_accept $ fdSocket $ Listen.listenSocket sock

    case r of
      -- this (EWOULDBLOCK) shouldn't happen (we just got told it was ready!),
      -- if it does (maybe the request got picked up by another thread) we'll
      -- just bail out
      -2 -> return ()
      -1 -> debugErrno "Backend.acceptCallback:c_accept()"
      fd -> do
          debug $ "acceptCallback: accept()ed fd, writing to chan " ++ show fd
          forkOnIO cpu $ (go r `catches` cleanup)
          return ()
  where
    go = runSession back handler sock
    cleanup = [ Handler $ \(_ :: TimeoutException) -> return ()
              , Handler $ \(e :: SomeException) ->
                  elog $ S.concat [ "libev.acceptCallback: "
                                  , S.pack . map c2w $ show e ]
              ]


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


stop :: Backend -> IO ()
stop b = ignoreException $ do
    debug $ "Backend.stop"

    -- 1. take the loop lock
    -- 2. shut down the accept() callback
    -- 3. call evUnloop and wake up the loop using evAsyncSend
    -- 4. release the loop lock, the main loop thread should then free/clean
    --    everything up (threads, connections, io objects, callbacks, etc)

    withMVar lock $ \_ -> do
        forM acceptObjs $ evIoStop loop
        evUnloop loop evunloop_all
        evAsyncSend loop killObj

  where
    loop           = _evLoop b
    acceptObjs     = _acceptIOObjs b
    killObj        = _killObj b
    lock           = _loopLock b

getAddr :: SockAddr -> IO (ByteString, Int)
getAddr addr =
    case addr of
      SockAddrInet p ha -> do
          s <- liftM (S.pack . map c2w) (inet_ntoa ha)
          return (s, fromIntegral p)

      a -> throwIO $ AddressNotSupportedException (show a)


-- | throw a timeout exception to the handling thread -- it'll clean up
-- everything
timerCallback :: EvLoopPtr         -- ^ loop obj
              -> EvTimerPtr        -- ^ timer obj
              -> IORef CTime       -- ^ when to timeout?
              -> ThreadId          -- ^ thread to kill
              -> TimerCallback
timerCallback loop tmr ioref tid _ _ _ = do
    debug "Backend.timerCallback: entered"

    now       <- getCurrentDateTime
    whenToDie <- readIORef ioref

    if whenToDie <= now
      then do
          debug "Backend.timerCallback: killing thread"
          throwTo tid TimeoutException

      else do
          debug $ "Backend.timerCallback: now=" ++ show now
                  ++ ", whenToDie=" ++ show whenToDie
          evTimerSetRepeat tmr $ fromRational . toRational $ (whenToDie - now)
          evTimerAgain loop tmr


-- if you already hold the loop lock, you are entitled to destroy a connection
destroyConnection :: Connection -> IO ()
destroyConnection conn = do
    debug "Backend.destroyConnection: closing socket and killing connection"
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

  where
    backend    = _backend conn
    loop       = _evLoop backend

    fd         = _rawSocket conn
    ioWrObj    = _connWriteIOObj conn
    ioWrCb     = _connWriteIOCallback conn
    ioRdObj    = _connReadIOObj conn
    ioRdCb     = _connReadIOCallback conn
    timerObj   = _timerObj conn
    timerCb    = _timerCallback conn


freeConnection :: Connection -> IO ()
freeConnection conn = ignoreException $ do
    withMVar loopLock $ \_ -> block $ do
        debug $ "freeConnection (" ++ show (_rawSocket conn) ++ ")"
        destroyConnection conn
        let tid = _connThread conn

        -- remove the thread id from the backend set
        H.delete tid $ _connectionThreads backend

        -- wake up the event loop so it can be apprised of the changes
        evAsyncSend loop asyncObj

  where
    backend    = _backend conn
    loop       = _evLoop backend
    loopLock   = _loopLock backend
    asyncObj   = _asyncObj backend


ignoreException :: IO () -> IO ()
ignoreException = handle (\(_::SomeException) -> return ())


freeBackend :: Backend -> IO ()
freeBackend backend = ignoreException $ block $ do
    -- note: we only get here after an unloop, so we have the loop lock
    -- here. (?)

    -- kill everything in thread table
    tset <- H.toList $ _connectionThreads backend

    let nthreads = Prelude.length tset

    debug $ "Backend.freeBackend: killing active connection threads"

    Prelude.mapM_ (destroyConnection . snd) tset

    -- kill the threads twice, they're probably getting stuck in the
    -- freeConnection 'finally' handler
    Prelude.mapM_ (killThread . fst) tset
    Prelude.mapM_ (killThread . fst) tset

    debug $ "Backend.freeBackend: " ++ show nthreads ++ " thread(s) killed"
    debug $ "Backend.freeBackend: destroying libev resources"

    mapM freeEvIo acceptObjs
    forM acceptCbs $ \x -> do
        acceptCb <- readMVar x
        freeIoCallback acceptCb

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
    acceptObjs  = _acceptIOObjs backend
    acceptCbs   = _acceptIOCallbacks backend
    asyncObj    = _asyncObj backend
    asyncCb     = _asyncCb backend
    killObj     = _killObj backend
    killCb      = _killCb backend
    (mcb1,mcb2) = _mutexCallbacks backend
    loop        = _evLoop backend


-- | Note: proc gets run in the background
runSession :: Backend 
           -> SessionHandler
           -> ListenSocket
           -> CInt
           -> IO ()
runSession backend handler lsock fd = do
        sock <- mkSocket fd AF_INET Stream 0 Connected
        peerName <- getPeerName sock
        sockName <- getSocketName sock
        tid <- myThreadId

        -- set_linger fd
        c_setnonblocking fd

        (raddr, rport) <- getAddr peerName
        (laddr, lport) <- getAddr sockName

        let lp = _evLoop backend

        -- makes sense to assume the socket is read/write available when
        -- opened; worst-case is we get EWOULDBLOCK
        ra    <- newMVar ()
        wa    <- newMVar ()


        -----------------
        -- setup timer --
        -----------------
        tmr         <- mkEvTimer
        now         <- getCurrentDateTime
        timeoutTime <- newIORef $ now + 20
        tcb         <- mkTimerCallback $ timerCallback lp
                                                      tmr
                                                      timeoutTime
                                                      tid
        -- 20 second timeout
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

        let sinfo = SessionInfo laddr lport raddr rport $ Listen.isSecure lsock
        let conn = Connection backend
                              lsock
                              fd
                              sinfo
                              ra
                              wa
                              tmr
                              tcb
                              timeoutTime
                              readActive
                              writeActive
                              evioRead
                              ioReadCb
                              evioWrite
                              ioWriteCb
                              tid

        bracket (Listen.createSession lsock bLOCKSIZE fd $
                       waitForLock True conn)
                (\session -> block $ do
                    debug "runSession: thread killed, closing socket"

                    eatException $ Listen.endSession lsock session
                    eatException $ freeConnection conn
                )
                (\session -> do H.update tid conn (_connectionThreads backend)
                                handler sinfo
                                        (enumerate conn session)
                                        (writeOut conn session)
                                        (sendFile conn session)
                                        (tickleTimeout conn)
                )

eatException :: IO a -> IO ()
eatException act = (act >> return ()) `catch` \(_::SomeException) -> return ()

data AddressNotSupportedException = AddressNotSupportedException String
   deriving (Typeable)

instance Show AddressNotSupportedException where
    show (AddressNotSupportedException x) = "Address not supported: " ++ x

instance Exception AddressNotSupportedException


------------------------------------------------------------------------------

bLOCKSIZE :: Int
bLOCKSIZE = 8192

--
-- About timeouts
--
-- It's not good enough to restart the timer from io(Read|Write)Callback,
-- because those seem to be edge-triggered. I've definitely had where after 20
-- seconds they still weren't being re-awakened.
--

data TimeoutException = TimeoutException
   deriving (Typeable)

instance Show TimeoutException where
    show _ = "timeout"

instance Exception TimeoutException

tickleTimeout :: Connection -> IO ()
tickleTimeout conn = do
    debug "Backend.tickleTimeout"
    now       <- getCurrentDateTime
    writeIORef (_timerTimeoutTime conn) (now + 30)

waitForLock :: Bool        -- ^ True = wait for read, False = wait for write
            -> Connection
            -> IO ()
waitForLock readLock conn = do
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

  where
    dbg s    = debug $ "Backend.recvData(" ++ show (_rawSocket conn) ++ "): "
                       ++ s
    io       = if readLock 
                 then (_connReadIOObj conn)
                 else (_connWriteIOObj conn)
    bk       = _backend conn
    active   = if readLock
                 then (_readActive conn)
                 else (_writeActive conn)
    lp       = _evLoop bk
    looplock = _loopLock bk
    async    = _asyncObj bk
    lock     = if readLock
                 then (_readAvailable conn)
                 else (_writeAvailable conn)

sendFile :: Connection -> NetworkSession -> FilePath -> Int64 -> Int64 -> IO ()
sendFile c s fp start sz = do
    withMVar lock $ \_ -> do
      act <- readIORef $ _writeActive c
      when act $ evIoStop loop io
      writeIORef (_writeActive c) False
      evAsyncSend loop asy

#if defined(HAS_SENDFILE)
    case (_listenSocket c) of
        ListenHttp _ -> bracket (openFd fp ReadOnly Nothing defaultFileFlags)
                                (closeFd)
                                (go start sz)
        _            -> do
            step <- runIteratee $ writeOut c s
            run_ $ enumFilePartial fp (start,start+sz) step
#else
    step <- runIteratee $ writeOut c s

    run_ $ enumFilePartial fp (start,start+sz) step
    return ()
#endif

    withMVar lock $ \_ -> do
      tryTakeMVar $ _readAvailable c
      tryTakeMVar $ _writeAvailable c
      evAsyncSend loop asy

  where
#if defined(HAS_SENDFILE)
    go off bytes fd
      | bytes == 0 = return ()
      | otherwise  = do
            sent <- SF.sendFile sfd fd off bytes
            if sent < bytes
              then tickleTimeout c >> go (off+sent) (bytes-sent) fd
              else return ()

    sfd  = Fd $ _rawSocket c
#endif
    io   = _connWriteIOObj c
    b    = _backend c
    loop = _evLoop b
    lock = _loopLock b
    asy  = _asyncObj b

enumerate :: (MonadIO m)
          => Connection
          -> NetworkSession
          -> Enumerator ByteString m a
enumerate conn session = loop
  where
    dbg s = debug $ "LibevBackend.enumerate(" ++ show (_socket session)
                    ++ "): " ++ s

    loop (Continue k) = do
        m <- liftIO $ recvData
        let s = fromMaybe "" m
        sendOne k s
    loop x = returnI x

    sendOne k s | S.null s  = do
        dbg "sending EOF to continuation"
        enumEOF $ Continue k

                | otherwise = do
        dbg $ "sending " ++ show s ++ " to continuation"
        step <- lift $ runIteratee $ k $ Chunks [s]
        case step of
          (Yield x st)   -> do
                      dbg $ "got yield, remainder is " ++ show st
                      yield x st
          r@(Continue _) -> do
                      dbg $ "got continue"
                      loop r
          (Error e)      -> throwError e

    recvData = Listen.recv (_listenSocket conn) (waitForLock True conn) session


writeOut :: (MonadIO m)
         => Connection
         -> NetworkSession
         -> Iteratee ByteString m ()
writeOut conn session = loop
  where
    loop = continue k

    k EOF = yield () EOF
    k (Chunks xs) = do
        liftIO $ sendData $ S.concat xs
        loop

    sendData = Listen.send (_listenSocket conn) 
                           (tickleTimeout conn)
                           (waitForLock False conn)
                           session

#endif
