{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE Rank2Types               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Snap.Internal.Http.Server.SimpleBackend
  ( simpleEventLoop
  ) where


------------------------------------------------------------------------------
import           Control.Monad.Trans

import           Control.Concurrent hiding (yield)
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (c2w)
import           Data.Maybe
import           Foreign hiding (new)
import           Foreign.C
import           GHC.Conc (labelThread, forkOnIO)
import           Network.Socket
import           Prelude hiding (catch)
------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Internal.Http.Server.Date
import qualified Snap.Internal.Http.Server.TimeoutManager as TM
import           Snap.Internal.Http.Server.TimeoutManager (TimeoutManager)
import           Snap.Internal.Http.Server.Backend
import           Snap.Internal.Http.Server.Address
import qualified Snap.Internal.Http.Server.ListenHelpers as Listen
import           Snap.Iteratee hiding (map)

#if defined(HAS_SENDFILE)
import qualified System.SendFile as SF
import           System.Posix.IO
import           System.Posix.Types (Fd(..))
#endif


------------------------------------------------------------------------------
-- | For each cpu, we store:
--    * A list of accept threads, one per port.
--    * A TimeoutManager
--    * An mvar to signal when the timeout thread is shutdown
data EventLoopCpu = EventLoopCpu
    { _boundCpu        :: Int
    , _acceptThreads   :: [ThreadId]
    , _timeoutManager  :: TimeoutManager
    , _exitMVar        :: !(MVar ())
    }


------------------------------------------------------------------------------
simpleEventLoop :: EventLoop
simpleEventLoop defaultTimeout sockets cap elog initial handler = do
    loops <- Prelude.mapM (newLoop defaultTimeout sockets handler elog)
                          [0..(cap-1)]

    initial
    debug "simpleEventLoop: waiting for mvars"

    --wait for all threads to exit
    Prelude.mapM_ (takeMVar . _exitMVar) loops `finally` do
        debug "simpleEventLoop: killing all threads"
        _ <- mapM_ stopLoop loops
        mapM_ Listen.closeSocket sockets


------------------------------------------------------------------------------
newLoop :: Int
        -> [ListenSocket]
        -> SessionHandler
        -> (S.ByteString -> IO ())
        -> Int
        -> IO EventLoopCpu
newLoop defaultTimeout sockets handler elog cpu = do
    tmgr       <- TM.initialize defaultTimeout getCurrentDateTime
    exit       <- newEmptyMVar
    accThreads <- forM sockets $ \p -> forkOnIO cpu $
                  acceptThread defaultTimeout handler tmgr elog cpu p exit

    return $! EventLoopCpu cpu accThreads tmgr exit


------------------------------------------------------------------------------
stopLoop :: EventLoopCpu -> IO ()
stopLoop loop = block $ do
    TM.stop $ _timeoutManager loop
    Prelude.mapM_ killThread $ _acceptThreads loop


------------------------------------------------------------------------------
acceptThread :: Int
             -> SessionHandler
             -> TimeoutManager
             -> (S.ByteString -> IO ())
             -> Int
             -> ListenSocket
             -> MVar ()
             -> IO ()
acceptThread defaultTimeout handler tmgr elog cpu sock exitMVar =
    loop `finally` (tryPutMVar exitMVar () >> return ())
  where
    acceptAndFork = do
        debug $ "acceptThread: calling accept() on socket " ++ show sock
        (s,addr) <- accept $ Listen.listenSocket sock
        setSocketOption s NoDelay 1
        debug $ "acceptThread: accepted connection from remote: " ++ show addr
        _ <- forkOnIO cpu (go s addr `catches` cleanup)
        return ()

    loop = do
        acceptAndFork `catches` acceptHandler
        loop

    go = runSession defaultTimeout handler tmgr sock

    acceptHandler =
        [ Handler $ \(e :: AsyncException) -> throwIO e
        , Handler $ \(e :: SomeException) -> do
              elog $ S.concat [ "SimpleBackend.acceptThread: accept threw: "
                              , S.pack . map c2w $ show e ]
              -- we're out of file descriptors, and it isn't likely to get
              -- better immediately; sleep for 10ms to avoid spamming the error
              -- log.
              threadDelay $ 10000
        ]

    cleanup =
        [
          Handler $ \(_ :: AsyncException) -> return ()
        , Handler $ \(e :: SomeException) -> elog
                  $ S.concat [ "SimpleBackend.acceptThread: "
                             , S.pack . map c2w $ show e]
        ]


------------------------------------------------------------------------------
runSession :: Int
           -> SessionHandler
           -> TimeoutManager
           -> ListenSocket
           -> Socket
           -> SockAddr -> IO ()
runSession defaultTimeout handler tmgr lsock sock addr = do
    let fd = fdSocket sock
    curId <- myThreadId

    debug $ "Backend.withConnection: running session: " ++ show addr
    labelThread curId $ "connHndl " ++ show fd

    (rport,rhost) <- getAddress addr
    (lport,lhost) <- getSocketName sock >>= getAddress

    let sinfo = SessionInfo lhost lport rhost rport $ Listen.isSecure lsock

    timeoutHandle <- TM.register (killThread curId) tmgr
    let modifyTimeout = TM.modify timeoutHandle
    let tickleTimeout = modifyTimeout . max

    bracket (Listen.createSession lsock 8192 fd
              (threadWaitRead $ fromIntegral fd))
            (\session -> block $ do
                 debug "thread killed, closing socket"

                 -- cancel thread timeout
                 TM.cancel timeoutHandle

                 eatException $ Listen.endSession lsock session
                 eatException $ shutdown sock ShutdownBoth
                 eatException $ sClose sock
            )
            (\s -> let writeEnd = writeOut lsock s sock
                                           (tickleTimeout defaultTimeout)
                   in handler sinfo
                              (enumerate lsock s sock)
                              writeEnd
                              (sendFile lsock (tickleTimeout defaultTimeout)
                                        fd writeEnd)
                              modifyTimeout
            )


------------------------------------------------------------------------------
eatException :: IO a -> IO ()
eatException act = (act >> return ()) `catch` \(_::SomeException) -> return ()


------------------------------------------------------------------------------
sendFile :: ListenSocket
         -> IO ()
         -> CInt
         -> Iteratee ByteString IO ()
         -> FilePath
         -> Int64
         -> Int64
         -> IO ()
#if defined(HAS_SENDFILE)
sendFile lsock tickle sock writeEnd fp start sz =
    case lsock of
        ListenHttp _ -> bracket (openFd fp ReadOnly Nothing defaultFileFlags)
                                (closeFd)
                                (go start sz)
        _            -> do
                   step <- runIteratee writeEnd
                   run_ $ enumFilePartial fp (start,start+sz) step
  where
    go off bytes fd
      | bytes == 0 = return ()
      | otherwise  = do
            sent <- SF.sendFile (threadWaitWrite $ fromIntegral sock)
                                sfd fd off bytes
            if sent < bytes
              then tickle >> go (off+sent) (bytes-sent) fd
              else return ()

    sfd = Fd sock
#else
sendFile _ _ _ writeEnd fp start sz = do
    -- no need to count bytes
    step <- runIteratee writeEnd
    run_ $ enumFilePartial fp (start,start+sz) step
    return ()
#endif


------------------------------------------------------------------------------
enumerate :: (MonadIO m)
          => ListenSocket
          -> NetworkSession
          -> Socket
          -> Enumerator ByteString m a
enumerate port session sock = loop
  where
    dbg s = debug $ "SimpleBackend.enumerate(" ++ show (_socket session)
            ++ "): " ++ s

    loop (Continue k) = do
        dbg "reading from socket"
        s <- liftIO $ timeoutRecv
        case s of
            Nothing -> do
                   dbg "got EOF from socket"
                   sendOne k ""
            Just s' -> do
                   dbg $ "got " ++ Prelude.show (S.length s')
                           ++ " bytes from read end"
                   sendOne k s'

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

    fd = fdSocket sock
#ifdef PORTABLE
    timeoutRecv = Listen.recv port sock (threadWaitRead $
                  fromIntegral fd) session
#else
    timeoutRecv = Listen.recv port (threadWaitRead $
                  fromIntegral fd) session
#endif


------------------------------------------------------------------------------
writeOut :: (MonadIO m)
         => ListenSocket
         -> NetworkSession
         -> Socket
         -> (IO ())
         -> Iteratee ByteString m ()
writeOut port session sock tickle = loop
  where
    dbg s = debug $ "SimpleBackend.writeOut(" ++ show (_socket session)
            ++ "): " ++ s

    loop = continue k

    k EOF = yield () EOF
    k (Chunks xs) = do
        let s = S.concat xs
        let n = S.length s
        dbg $ "got chunk with " ++ show n ++ " bytes"
        ee <- liftIO $ try $ timeoutSend s
        case ee of
          (Left (e::SomeException)) -> do
              dbg $ "timeoutSend got error " ++ show e
              throwError e
          (Right _) -> do
              let last10 = S.drop (n-10) s
              dbg $ "wrote " ++ show n ++ " bytes, last 10=" ++ show last10
              loop

    fd = fdSocket sock
#ifdef PORTABLE
    timeoutSend = Listen.send port sock tickle
                              (threadWaitWrite $ fromIntegral fd) session
#else
    timeoutSend = Listen.send port tickle
                              (threadWaitWrite $ fromIntegral fd) session
#endif
