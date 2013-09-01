{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.TimeoutManager
  ( TimeoutManager
  , TimeoutHandle
  , initialize
  , stop
  , register
  , tickle
  , set
  , modify
  , cancel
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Foreign.C.Types
import           Prelude                          (Bool, IO, Int, const,
                                                   fromEnum, fromIntegral, id,
                                                   max, null, otherwise,
                                                   toEnum, ($), ($!), (+),
                                                   (++), (-), (.), (<=), (==))
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Common (eatException)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
type State = CTime

canceled :: State
canceled = 0

isCanceled :: State -> Bool
isCanceled = (== 0)


------------------------------------------------------------------------------
data TimeoutHandle = TimeoutHandle {
      _killAction :: IO ()
    , _state      :: IORef State
    , _hGetTime   :: IO CTime
    }


------------------------------------------------------------------------------
-- | Given a 'State' value and the current time, apply the given modification
-- function to the amount of time remaining.
--
smap :: CTime -> (Int -> Int) -> State -> State
smap now f deadline | isCanceled deadline = canceled
                    | otherwise = t'
  where
    remaining    = fromEnum $ max 0 (deadline - now)
    newremaining = f remaining
    t'           = now + fromIntegral newremaining


------------------------------------------------------------------------------
data TimeoutManager = TimeoutManager {
      _defaultTimeout :: !Int
    , _getTime        :: !(IO CTime)
    , _connections    :: !(IORef [TimeoutHandle])
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
    mp    <- newEmptyMVar
    mthr  <- newEmptyMVar

    let tm = TimeoutManager defaultTimeout getTime conns mp mthr

    thr <- mask $ \restore -> forkIO $ managerThread tm restore
    putMVar mthr thr
    return tm


------------------------------------------------------------------------------
-- | Stop a TimeoutManager.
stop :: TimeoutManager -> IO ()
stop tm = readMVar (_managerThread tm) >>= killThread


------------------------------------------------------------------------------
wakeup :: TimeoutManager -> IO ()
wakeup tm = void $ tryPutMVar morePlease $! ()
  where
    morePlease     = _morePlease tm


------------------------------------------------------------------------------
-- | Register a new connection with the TimeoutManager.
register :: IO ()
         -- ^ action to run when the timeout deadline is exceeded.
         -> TimeoutManager   -- ^ manager to register with.
         -> IO TimeoutHandle
register killAction tm = do
    now <- getTime
    let !state = now + toEnum defaultTimeout
    stateRef <- newIORef state

    let !h = TimeoutHandle killAction stateRef getTime
    !_ <- atomicModifyIORef connections $ \x -> (h:x, ())

    wakeup tm
    return h

  where
    getTime        = _getTime tm
    connections    = _connections tm
    defaultTimeout = _defaultTimeout tm


------------------------------------------------------------------------------
-- | Tickle the timeout on a connection to be at least N seconds into the
-- future. If the existing timeout is set for M seconds from now, where M > N,
-- then the timeout is unaffected.
tickle :: TimeoutHandle -> Int -> IO ()
tickle th = modify th . max
{-# INLINE tickle #-}


------------------------------------------------------------------------------
-- | Set the timeout on a connection to be N seconds into the future.
set :: TimeoutHandle -> Int -> IO ()
set th = modify th . const
{-# INLINE set #-}


------------------------------------------------------------------------------
-- | Modify the timeout with the given function.
modify :: TimeoutHandle -> (Int -> Int) -> IO ()
modify th f = do
    now   <- getTime
    state <- readIORef stateRef
    let !state' = smap now f state
    writeIORef stateRef state'

  where
    getTime  = _hGetTime th
    stateRef = _state th
{-# INLINE modify #-}


------------------------------------------------------------------------------
-- | Cancel a timeout.
cancel :: TimeoutHandle -> IO ()
cancel h = _killAction h >> writeIORef (_state h) canceled
{-# INLINE cancel #-}


------------------------------------------------------------------------------
managerThread :: TimeoutManager
              -> (forall a. IO a -> IO a)
              -> IO ()
managerThread tm restore =
    loop `finally` (readIORef connections >>= destroyAll)
  where
    --------------------------------------------------------------------------
    connections = _connections tm
    getTime     = _getTime tm
    morePlease  = _morePlease tm
    waitABit    = threadDelay 1000000

    --------------------------------------------------------------------------
    loop = do
        restore waitABit
        handles <- atomicModifyIORef connections (\x -> ([], x))
        if null handles
          then restore $ takeMVar morePlease
          else do
            (keeps, discards) <- process handles
            atomicModifyIORef connections (\x -> (keeps x, ())) >>= evaluate
            mapM_ (eatException . _killAction) $ discards []
        loop

    --------------------------------------------------------------------------
    process handles = do
        now   <- getTime
        processHandles now handles

    --------------------------------------------------------------------------
    processHandles now handles = go handles id id
      where
        go [] !keeps !discards = return (keeps, discards)

        go (x:xs) !keeps !discards = do
            state   <- readIORef $ _state x
            (!k',!d') <- if isCanceled state
                           then return (keeps, discards)
                           else if state <= now
                                  then return (keeps, discards . (x:))
                                  else return (keeps . (x:), discards)
            go xs k' d'

    --------------------------------------------------------------------------
    destroyAll = mapM_ diediedie

    --------------------------------------------------------------------------
    diediedie x = do
        state <- readIORef $ _state x
        if isCanceled state
          then return $! ()
          else _killAction x
