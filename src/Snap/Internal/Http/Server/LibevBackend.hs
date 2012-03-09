{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
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
libEvEventLoop _ _ _ _ _ = throwIO $
    LibevException "libev event loop is not supported"

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
import             Snap.Internal.Http.Server.Address
import qualified   Snap.Internal.Http.Server.ListenHelpers as Listen

#if defined(HAS_SENDFILE)
import qualified   System.SendFile as SF
import             System.Posix.IO
import             System.Posix.Types (Fd(..))
#endif


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
libEvEventLoop :: EventLoop
libEvEventLoop defaultTimeout sockets cap elog handler = do
    backends <- Prelude.mapM (newLoop defaultTimeout sockets handler elog)
                             [0..(cap-1)]

    debug "libevEventLoop: waiting for loop exit"
    ignoreException (Prelude.mapM_ (takeMVar . _loopExit) backends)
    debug "libevEventLoop: stopping all backends"
    ignoreException $ mapM stop backends
    ignoreException $ mapM Listen.closeSocket sockets


------------------------------------------------------------------------------
newLoop :: Int                   -- ^ default timeout
        -> [ListenSocket]        -- ^ value you got from bindIt
        -> SessionHandler        -- ^ handler
        -> (ByteString -> IO ()) -- ^ error logger
        -> Int                   -- ^ cpu
        -> IO Backend
newLoop defaultTimeout sockets handler elog cpu = do
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

    -- setup the accept callback; this watches for read readiness on the
    -- listen port
    forM_ (zip3 sockets accIOs accMVars) $ \(sock, accIO, x) -> do
        accCB <- mkIoCallback $ acceptCallback defaultTimeout b handler elog
                                               cpu sock
        evIoInit accIO accCB (fdSocket $ Listen.listenSocket sock) ev_read
        evIoStart lp accIO
        putMVar x accCB

    forkOnIO cpu $ loopThread b

    debug $ "LibEv.newLoop: loop spawned"
    return b


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
acceptCallback :: Int
               -> Backend
               -> SessionHandler
               -> (ByteString -> IO ())
               -> Int
               -> ListenSocket
               -> IoCallback
acceptCallback defaultTimeout back handler
               elog cpu sock _loopPtr _ioPtr _ = do
    debug "inside acceptCallback"
    r <- c_accept $ fdSocket $ Listen.listenSocket sock

    case r of
      -- this (EWOULDBLOCK) shouldn't happen (we just got told it was ready!),
      -- if it does (maybe the request got picked up by another thread) we'll
      -- just bail out
      -2 -> return ()
      -1 -> debugErrno "Libev.acceptCallback:c_accept()"
      fd -> do
          debug $ "acceptCallback: accept()ed fd, writing to chan " ++ show fd
          forkOnIO cpu $ (go r `catches` cleanup)
          return ()
  where
    go = runSession defaultTimeout back handler sock
    cleanup = [ Handler $ \(_ :: TimeoutException) -> return ()
              , Handler $ \(_ :: AsyncException)   -> return ()
              , Handler $ \(e :: SomeException) ->
                  elog $ S.concat [ "libev.acceptCallback: "
                                  , S.pack . map c2w $ show e ]
              ]


------------------------------------------------------------------------------
ioReadCallback :: CInt -> IORef Bool -> MVar () -> IoCallback
ioReadCallback fd active ra _loopPtr _ioPtr _ = do
    -- send notifications to the worker thread
    debug $ "ioReadCallback: notification (" ++ show fd ++ ")"
    tryPutMVar ra ()
    debug $ "stopping ioReadCallback (" ++ show fd ++ ")"
    evIoStop _loopPtr _ioPtr
    writeIORef active False


------------------------------------------------------------------------------
ioWriteCallback :: CInt -> IORef Bool -> MVar () -> IoCallback
ioWriteCallback fd active wa _loopPtr _ioPtr _ = do
    -- send notifications to the worker thread
    debug $ "ioWriteCallback: notification (" ++ show fd ++ ")"
    tryPutMVar wa ()
    debug $ "stopping ioWriteCallback (" ++ show fd ++ ")"
    evIoStop _loopPtr _ioPtr
    writeIORef active False


------------------------------------------------------------------------------
stop :: Backend -> IO ()
stop b = ignoreException $ do
    debug $ "Libev.stop"

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


------------------------------------------------------------------------------
-- | Throws a timeout exception to the handling thread.  The thread will clean
-- up everything.
timerCallback :: EvLoopPtr         -- ^ loop obj
              -> EvTimerPtr        -- ^ timer obj
              -> IORef CTime       -- ^ when to timeout?
              -> ThreadId          -- ^ thread to kill
              -> TimerCallback
timerCallback loop tmr ioref tid _ _ _ = do
    debug "Libev.timerCallback: entered"

    now       <- getCurrentDateTime
    whenToDie <- readIORef ioref

    if whenToDie <= now
      then do
          debug "Libev.timerCallback: killing thread"
          throwTo tid TimeoutException

      else do
          debug $ "Libev.timerCallback: now=" ++ show now
                  ++ ", whenToDie=" ++ show whenToDie
          evTimerSetRepeat tmr $ fromRational . toRational $ (whenToDie - now)
          evTimerAgain loop tmr


------------------------------------------------------------------------------
-- | If you already hold the loop lock, you are entitled to destroy a
-- connection
destroyConnection :: Connection -> IO ()
destroyConnection conn = do
    debug "Libev.destroyConnection: closing socket and killing connection"
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


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
freeBackend :: Backend -> IO ()
freeBackend backend = ignoreException $ block $ do
    -- note: we only get here after an unloop, so we have the loop lock
    -- here. (?)

    -- kill everything in thread table
    tset <- H.toList $ _connectionThreads backend

    let nthreads = Prelude.length tset

    debug $ "Libev.freeBackend: killing active connection threads"

    Prelude.mapM_ (destroyConnection . snd) tset

    -- kill the threads twice, they're probably getting stuck in the
    -- freeConnection 'finally' handler
    Prelude.mapM_ (killThread . fst) tset
    Prelude.mapM_ (killThread . fst) tset

    debug $ "Libev.freeBackend: " ++ show nthreads ++ " thread(s) killed"
    debug $ "Libev.freeBackend: destroying libev resources"

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
    debug $ "Libev.freeBackend: resources destroyed"

  where
    acceptObjs  = _acceptIOObjs backend
    acceptCbs   = _acceptIOCallbacks backend
    asyncObj    = _asyncObj backend
    asyncCb     = _asyncCb backend
    killObj     = _killObj backend
    killCb      = _killCb backend
    (mcb1,mcb2) = _mutexCallbacks backend
    loop        = _evLoop backend


------------------------------------------------------------------------------
-- | Note: proc gets run in the background
runSession :: Int
           -> Backend
           -> SessionHandler
           -> ListenSocket
           -> CInt
           -> IO ()
runSession defaultTimeout backend handler lsock fd = do
    sock <- mkSocket fd AF_INET Stream 0 Connected
    peerName <- getPeerName sock
    sockName <- getSocketName sock
    tid <- myThreadId

    -- set_linger fd
    c_setnonblocking fd

    (rport, raddr) <- getAddress peerName
    (lport, laddr) <- getAddress sockName

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
    timeoutTime <- newIORef $ now + (fromIntegral defaultTimeout)
    tcb         <- mkTimerCallback $ timerCallback lp
                                                  tmr
                                                  timeoutTime
                                                  tid
    evTimerInit tmr tcb 0 (fromIntegral defaultTimeout)


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

    let sinfo = SessionInfo laddr lport raddr rport $
                Listen.isSecure lsock
    let conn  = Connection backend
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

    go sinfo conn `finally` (block $ do
        debug "runSession: thread finished, freeing connection"
        ignoreException $ freeConnection conn)

  where
    go sinfo conn =
        bracket (Listen.createSession lsock bLOCKSIZE fd $
                     waitForLock True conn)
                (\session -> block $ do
                    debug "runSession: session finished, cleaning up"
                    ignoreException $ Listen.endSession lsock session
                )
                (\session -> do H.update (_connThread conn)
                                         conn
                                         (_connectionThreads backend)
                                handler sinfo
                                      (enumerate conn session)
                                      (writeOut defaultTimeout conn session)
                                      (sendFile defaultTimeout conn session)
                                      (setTimeout conn)
                )


------------------------------------------------------------------------------
ignoreException :: IO a -> IO ()
ignoreException act =
    (act >> return ()) `catch` \(_::SomeException) -> return ()


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
data TimeoutException = TimeoutException
   deriving (Typeable)

instance Show TimeoutException where
    show _ = "timeout"

instance Exception TimeoutException


------------------------------------------------------------------------------
modifyTimeout :: Connection -> (Int -> Int) -> IO ()
modifyTimeout conn f = do
    debug "Libev.modifyTimeout"
    !prev <- readIORef tt
    !now  <- getCurrentDateTime

    let !remaining    = fromEnum $ max 0 (prev - now)
    let !newRemaining = f remaining
    let !newTimeout   = now + toEnum newRemaining

    writeIORef tt $! now + toEnum newRemaining

    -- Here the question is: do we reset the ev timer? If we're extending the
    -- timeout, the ev manual suggests it's more efficient to let the timer
    -- lapse and re-arm. If we're shortening the timeout, we need to update the
    -- timer so it fires when it's supposed to.
    when (newTimeout < prev) $ withMVar loopLock $ \_ -> do
        evTimerSetRepeat tmr $! toEnum newRemaining
        evTimerAgain loop tmr
        -- wake up the event loop so it can be apprised of the changes
        evAsyncSend loop asyncObj

  where
    tt       = _timerTimeoutTime conn
    backend  = _backend conn
    asyncObj = _asyncObj backend
    loopLock = _loopLock backend
    loop     = _evLoop backend
    tmr      = _timerObj conn


------------------------------------------------------------------------------
tickleTimeout :: Connection -> Int -> IO ()
tickleTimeout conn = modifyTimeout conn . max


------------------------------------------------------------------------------
setTimeout :: Connection -> Int -> IO ()
setTimeout conn = modifyTimeout conn . const


------------------------------------------------------------------------------
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
    dbg s    = debug $ "Libev.recvData(" ++ show (_rawSocket conn) ++ "): "
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


------------------------------------------------------------------------------
sendFile :: Int
         -> Connection
         -> NetworkSession
         -> FilePath
         -> Int64
         -> Int64
         -> IO ()
sendFile defaultTimeout c s fp start sz = do
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
            step <- runIteratee $ writeOut defaultTimeout c s
            run_ $ enumFilePartial fp (start,start+sz) step
#else
    step <- runIteratee $ writeOut defaultTimeout c s

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
            sent <- SF.sendFile (waitForLock False c) sfd fd off bytes
            if sent < bytes
              then tickleTimeout c defaultTimeout >>
                   go (off+sent) (bytes-sent) fd
              else return ()

    sfd  = Fd $ _rawSocket c
#endif
    io   = _connWriteIOObj c
    b    = _backend c
    loop = _evLoop b
    lock = _loopLock b
    asy  = _asyncObj b


------------------------------------------------------------------------------
enumerate :: (MonadIO m)
          => Connection
          -> NetworkSession
          -> Enumerator ByteString m a
enumerate conn session = loop
  where
    dbg s = debug $ "Libev.enumerate(" ++ show (_socket session)
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

    recvData = Listen.recv (_listenSocket conn)
                           (waitForLock True conn) session


------------------------------------------------------------------------------
writeOut :: (MonadIO m)
         => Int
         -> Connection
         -> NetworkSession
         -> Iteratee ByteString m ()
writeOut defaultTimeout conn session = loop
  where
    loop = continue k

    k EOF = yield () EOF
    k (Chunks xs) = do
        liftIO $ sendData $ S.concat xs
        loop

    sendData = Listen.send (_listenSocket conn)
                           (tickleTimeout conn defaultTimeout)
                           (waitForLock False conn)
                           session

#endif
