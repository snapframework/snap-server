{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Snap.Internal.Http.Server.SimpleBackend
  ( simpleEventLoop
  ) where

------------------------------------------------------------------------------
import "monads-fd" Control.Monad.Trans

import           Control.Concurrent hiding (yield)
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (c2w)
import           Data.Maybe
import           Data.Typeable
import           Data.Word
import           Foreign hiding (new)
import           Foreign.C
import           GHC.Conc (labelThread, forkOnIO)
import           Network.Socket
import           Prelude hiding (catch)
------------------------------------------------------------------------------
import           Data.Concurrent.HashMap (hashString)
import           Snap.Internal.Debug
import           Snap.Internal.Http.Server.Date
import qualified Snap.Internal.Http.Server.TimeoutTable as TT
import           Snap.Internal.Http.Server.TimeoutTable (TimeoutTable)
import           Snap.Internal.Http.Server.Backend
import qualified Snap.Internal.Http.Server.ListenHelpers as Listen
import           Snap.Iteratee hiding (map)

#if defined(HAS_SENDFILE)
import qualified System.SendFile as SF
import           System.Posix.IO
import           System.Posix.Types (Fd(..))
#endif


------------------------------------------------------------------------------
{- For each cpu, we store:
    + a list of accept threads, one per port.
    + one timeout table and one timeout thread.  These timeout the session threads.
    + a mvar to signal when the timeout thread is shutdown
-}
data EventLoopCpu = EventLoopCpu
    { _boundCpu        :: Int
    , _acceptThreads   :: [ThreadId]
    , _timeoutTable    :: TimeoutTable
    , _timeoutThread   :: ThreadId
    , _exitMVar        :: !(MVar ())
    }

simpleEventLoop :: EventLoop
simpleEventLoop sockets cap elog handler = do
    loops <- Prelude.mapM (newLoop sockets handler elog) [0..(cap-1)]

    debug "simpleEventLoop: waiting for mvars"
    
    --wait for all threads to exit
    Prelude.mapM_ (takeMVar . _exitMVar) loops `finally` do
        debug "simpleEventLoop: killing all threads"
        mapM stopLoop loops
        mapM Listen.closeSocket sockets

newLoop :: [ListenSocket]
        -> SessionHandler
        -> (S.ByteString -> IO ())
        -> Int
        -> IO EventLoopCpu
newLoop sockets handler elog cpu = do
    tt         <- TT.new
    exit       <- newEmptyMVar
    accThreads <- forM sockets $ \p -> forkOnIO cpu $ acceptThread handler tt elog cpu p
    tid        <- forkOnIO cpu $ timeoutThread tt exit

    return $ EventLoopCpu cpu accThreads tt tid exit

stopLoop :: EventLoopCpu -> IO ()
stopLoop loop = block $ do
    Prelude.mapM_ killThread $ _acceptThreads loop
    killThread $ _timeoutThread loop

acceptThread :: SessionHandler 
             -> TimeoutTable 
             -> (S.ByteString -> IO ()) 
             -> Int 
             -> ListenSocket 
             -> IO ()
acceptThread handler tt elog cpu sock = loop
  where
    loop = do
        debug $ "acceptThread: calling accept()"
        (s,addr) <- accept $ Listen.listenSocket sock
        debug $ "acceptThread: accepted connection from remote: " ++ show addr
        forkOnIO cpu (go s addr `catches` cleanup)
        loop

    go = runSession handler tt sock
 
    cleanup = [
                Handler $ \(e :: SomeException) -> elog $ S.concat [ "SimpleBackend.acceptThread: ", S.pack . map c2w $ show e]
              ]

timeoutThread :: TimeoutTable -> MVar () -> IO ()
timeoutThread table exitMVar = do
    go `catch` (\(_::SomeException) -> killAll)
    putMVar exitMVar ()

  where
    go = do
        debug "timeoutThread: waiting for activity on thread table"
        TT.waitForActivity table
        debug "timeoutThread: woke up, killing old connections"
        killTooOld
        go


    killTooOld = do
        now    <- getCurrentDateTime
        TT.killOlderThan (now - tIMEOUT) table

    -- timeout = 30 seconds
    tIMEOUT = 30

    killAll = do
        debug "Backend.timeoutThread: shutdown, killing all connections"
        TT.killAll table


data AddressNotSupportedException = AddressNotSupportedException String
   deriving (Typeable)

instance Show AddressNotSupportedException where
    show (AddressNotSupportedException x) = "Address not supported: " ++ x

instance Exception AddressNotSupportedException


runSession :: SessionHandler -> TimeoutTable -> ListenSocket -> Socket -> SockAddr -> IO ()
runSession handler tt lsock sock addr = do
    let fd = fdSocket sock
    curId <- myThreadId

    debug $ "Backend.withConnection: running session: " ++ show addr
    labelThread curId $ "connHndl " ++ show fd

    (rport,rhost) <-
        case addr of
          SockAddrInet p h -> do
             h' <- inet_ntoa h
             return (fromIntegral p, S.pack $ map c2w h')
          x -> throwIO $ AddressNotSupportedException $ show x

    laddr <- getSocketName sock

    (lport,lhost) <-
        case laddr of
          SockAddrInet p h -> do
             h' <- inet_ntoa h
             return (fromIntegral p, S.pack $ map c2w h')
          x -> throwIO $ AddressNotSupportedException $ show x

    let sinfo = SessionInfo lhost lport rhost rport $ Listen.isSecure lsock
    let curHash = hashString $ show curId
    let timeout = tickleTimeout tt curId curHash

    timeout

    bracket (Listen.createSession lsock 8192 fd (threadWaitRead $ fromIntegral fd))
            (\session -> block $ do
                 debug "thread killed, closing socket"

                 -- remove thread from timeout table
                 TT.delete curHash curId tt

                 eatException $ Listen.endSession lsock session
                 eatException $ shutdown sock ShutdownBoth
                 eatException $ sClose sock
            )
            (\s -> let writeEnd = writeOut lsock s sock timeout
                   in handler sinfo 
                              (enumerate lsock s sock)
                              writeEnd
                              (sendFile lsock timeout fd writeEnd)
                              timeout
            )

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
            sent <- SF.sendFile sfd fd off bytes
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

tickleTimeout :: TimeoutTable -> ThreadId -> Word -> IO ()
tickleTimeout table tid thash = do
    debug "Backend.tickleTimeout"
    now   <- getCurrentDateTime
    TT.insert thash tid now table

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
    timeoutRecv = Listen.recv port sock (threadWaitRead $ fromIntegral fd) session
#else
    timeoutRecv = Listen.recv port (threadWaitRead $ fromIntegral fd) session
#endif


writeOut :: (MonadIO m)
         => ListenSocket
         -> NetworkSession
         -> Socket
         -> IO ()
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
    timeoutSend = Listen.send port sock tickle (threadWaitWrite $ fromIntegral fd) session
#else
    timeoutSend = Listen.send port tickle (threadWaitWrite $ fromIntegral fd) session
#endif
