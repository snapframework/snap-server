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

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as B
import           Data.ByteString.Internal (c2w)
import           Data.Iteratee.WrappedByteString
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
import           Snap.Iteratee hiding (foldl')

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
        -> (B.ByteString -> IO ())
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
             -> (B.ByteString -> IO ()) 
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
                Handler $ \(e :: SomeException) -> elog $ B.concat [ "SimpleBackend.acceptThread: ", B.pack . map c2w $ show e]
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
             return (fromIntegral p, B.pack $ map c2w h')
          x -> throwIO $ AddressNotSupportedException $ show x

    laddr <- getSocketName sock

    (lport,lhost) <-
        case laddr of
          SockAddrInet p h -> do
             h' <- inet_ntoa h
             return (fromIntegral p, B.pack $ map c2w h')
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
sendFile :: ListenSocket -> IO () -> CInt -> Iteratee IO () -> FilePath -> Int64 -> Int64 -> IO ()
#if defined(HAS_SENDFILE)
sendFile lsock tickle sock writeEnd fp start sz =
    case lsock of
        ListenHttp _ -> bracket (openFd fp ReadOnly Nothing defaultFileFlags)
                                (closeFd)
                                (go start sz)
        _            -> enumFilePartial fp (start,start+sz) writeEnd >>= run
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
    enumFilePartial fp (start,start+sz) writeEnd >>= run
    return ()
#endif

tickleTimeout :: TimeoutTable -> ThreadId -> Word -> IO ()
tickleTimeout table tid thash = do
    debug "Backend.tickleTimeout"
    now   <- getCurrentDateTime
    TT.insert thash tid now table

enumerate :: (MonadIO m) => ListenSocket -> NetworkSession -> Socket -> Enumerator m a
enumerate port session sock = loop
  where
    loop f = do
        debug $ "Backend.enumerate: reading from socket"
        s <- liftIO $ timeoutRecv
        case s of
            Nothing -> debug "Backend.enumerate: connection closed"
            Just s' -> debug $ "Backend.enumerate: got " ++ Prelude.show (B.length s')
                             ++ " bytes from read end"
        sendOne f s

    sendOne f s = do
        v <- runIter f (if isNothing s
                         then EOF Nothing
                         else Chunk $ WrapBS $ fromJust s)
        case v of
          r@(Done _ _)      -> return $ liftI r
          (Cont k Nothing)  -> loop k
          (Cont _ (Just e)) -> return $ throwErr e

    fd = fdSocket sock
#ifdef PORTABLE
    timeoutRecv = Listen.recv port sock (threadWaitRead $ fromIntegral fd) session
#else
    timeoutRecv = Listen.recv port (threadWaitRead $ fromIntegral fd) session
#endif


writeOut :: (MonadIO m) => ListenSocket -> NetworkSession -> Socket -> IO () -> Iteratee m ()
writeOut port session sock tickle = iteratee
  where
    iteratee = IterateeG out

    out c@(EOF _)   = return $ Done () c

    out (Chunk s) = do
        let x = unWrap s

        debug $ "Backend.writeOut: writing data " ++ show (B.length x)
        ee <- liftIO $ ((try $ timeoutSend x)
                            :: IO (Either SomeException ()))

        case ee of
          (Left e)  -> do debug $ "Backend.writeOut: received error " ++ Prelude.show e
                          return $ Done () (EOF $ Just $ Err $ show e)
          (Right _) -> do debug "Backend.writeOut: successfully sent data"
                          return $ Cont iteratee Nothing

    fd = fdSocket sock
#ifdef PORTABLE
    timeoutSend = Listen.send port sock tickle (threadWaitWrite $ fromIntegral fd) session
#else
    timeoutSend = Listen.send port tickle (threadWaitWrite $ fromIntegral fd) session
#endif
