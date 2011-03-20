{-# LANGUAGE BangPatterns #-}

module Snap.Internal.Http.Server.TimeoutManager
  ( TimeoutManager
  , TimeoutHandle
  , initialize
  , stop
  , register
  , tickle
  , cancel
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Foreign.C.Types

------------------------------------------------------------------------------
data State = Deadline !CTime
           | Canceled


------------------------------------------------------------------------------
data TimeoutHandle = TimeoutHandle {
      _killAction :: !(IO ())
    , _state      :: !(IORef State)
    , _hGetTime   :: !(IO CTime)
    }


------------------------------------------------------------------------------
data TimeoutManager = TimeoutManager {
      _defaultTimeout :: !Int
    , _getTime        :: !(IO CTime)
    , _connections    :: !(IORef [TimeoutHandle])
    , _inactivity     :: !(IORef Bool)
    , _morePlease     :: !(MVar ())
    , _managerThread  :: !(MVar ThreadId)
    }


------------------------------------------------------------------------------
-- | Create a new TimeoutManager.
initialize :: Int               -- ^ default timeout
           -> IO CTime          -- ^ function to get current time
           -> IO TimeoutManager
initialize defaultTimeout getTime = do
    conns <- newIORef []
    inact <- newIORef False
    mp    <- newEmptyMVar
    mthr  <- newEmptyMVar

    let tm = TimeoutManager defaultTimeout getTime conns inact mp mthr

    thr <- forkIO $ managerThread tm
    putMVar mthr thr
    return tm


------------------------------------------------------------------------------
-- | Stop a TimeoutManager.
stop :: TimeoutManager -> IO ()
stop tm = readMVar (_managerThread tm) >>= killThread


------------------------------------------------------------------------------
-- | Register a new connection with the TimeoutManager.
register :: IO ()               -- ^ action to run when the timeout deadline is
                                -- exceeded.
         -> TimeoutManager      -- ^ manager to register with.
         -> IO TimeoutHandle
register killAction tm = do
    now <- getTime
    let !state = Deadline $ now + toEnum defaultTimeout
    stateRef <- newIORef state

    let !h = TimeoutHandle killAction stateRef getTime
    atomicModifyIORef connections $ \x -> (h:x, ())

    inact <- readIORef inactivity
    when inact $ do
        -- wake up manager thread
        writeIORef inactivity False
        _ <- tryPutMVar morePlease ()
        return ()
    return h

  where
    getTime        = _getTime tm
    inactivity     = _inactivity tm
    morePlease     = _morePlease tm
    connections    = _connections tm
    defaultTimeout = _defaultTimeout tm


------------------------------------------------------------------------------
-- | Tickle the timeout on a connection to be N seconds into the future.
tickle :: TimeoutHandle -> Int -> IO ()
tickle th n = do
    now <- getTime

    let state = Deadline $ now + toEnum n
    writeIORef stateRef state

  where
    getTime  = _hGetTime th
    stateRef = _state th


------------------------------------------------------------------------------
-- | Cancel a timeout.
cancel :: TimeoutHandle -> IO ()
cancel h = writeIORef (_state h) Canceled


------------------------------------------------------------------------------
managerThread :: TimeoutManager -> IO ()
managerThread tm = loop `finally` (readIORef connections >>= destroyAll)
  where
    --------------------------------------------------------------------------
    connections = _connections tm
    getTime     = _getTime tm
    inactivity  = _inactivity tm
    morePlease  = _morePlease tm
    waitABit    = threadDelay 5000000

    --------------------------------------------------------------------------
    loop = do
        waitABit
        handles <- atomicModifyIORef connections (\x -> ([],x))

        if null handles
          then do
            -- we're inactive, go to sleep until we get new threads
            writeIORef inactivity True
            takeMVar morePlease
          else do
            now   <- getTime
            dlist <- processHandles now handles id
            atomicModifyIORef connections (\x -> (dlist x, ()))

        loop

    --------------------------------------------------------------------------
    processHandles !now handles initDlist = go handles initDlist
      where
        go [] !dlist = return dlist

        go (x:xs) !dlist = do
            state   <- readIORef $ _state x
            !dlist' <- case state of
                         Canceled   -> return dlist
                         Deadline t -> if t <= now
                                         then do
                                           _killAction x
                                           return dlist
                                         else return (dlist . (x:))
            go xs dlist'

    --------------------------------------------------------------------------
    destroyAll = mapM_ diediedie

    --------------------------------------------------------------------------
    diediedie x = do
        state <- readIORef $ _state x
        case state of
          Canceled -> return ()
          _        -> _killAction x
