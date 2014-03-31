{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

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
import           Control.Concurrent.Extended (forkIOLabeledWithUnmaskBs)
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Foreign.C.Types

------------------------------------------------------------------------------
data State = Deadline !CTime
           | Canceled
  deriving (Eq, Show)


------------------------------------------------------------------------------
instance Ord State where
    compare Canceled Canceled         = EQ
    compare Canceled _                = LT
    compare _        Canceled         = GT
    compare (Deadline a) (Deadline b) = compare a b


------------------------------------------------------------------------------
-- Probably breaks Num laws, but I can live with it
--
instance Num State where
    --------------------------------------------------------------------------
    Canceled     + Canceled     = Canceled
    Canceled     + x            = x
    x            + Canceled     = x
    (Deadline a) + (Deadline b) = Deadline $! a + b

    --------------------------------------------------------------------------
    Canceled     - Canceled     = Canceled
    Canceled     - x            = negate x
    x            - Canceled     = x
    (Deadline a) - (Deadline b) = Deadline $! a - b

    --------------------------------------------------------------------------
    Canceled     * _            = Canceled
    _            * Canceled     = Canceled
    (Deadline a) * (Deadline b) = Deadline $! a * b

    --------------------------------------------------------------------------
    negate Canceled     = Canceled
    negate (Deadline d) = Deadline (negate d)

    --------------------------------------------------------------------------
    abs Canceled     = Canceled
    abs (Deadline d) = Deadline (abs d)

    --------------------------------------------------------------------------
    signum Canceled     = Canceled
    signum (Deadline d) = Deadline (signum d)

    --------------------------------------------------------------------------
    fromInteger = Deadline . fromInteger


------------------------------------------------------------------------------
data TimeoutHandle = TimeoutHandle {
      _killAction :: !(IO ())
    , _state      :: !(IORef State)
    , _hGetTime   :: !(IO CTime)
    }


------------------------------------------------------------------------------
-- | Given a 'State' value and the current time, apply the given modification
-- function to the amount of time remaining.
--
smap :: CTime -> (Int -> Int) -> State -> State
smap _ _ Canceled       = Canceled

smap now f (Deadline t) = Deadline t'
  where
    !remaining    = fromEnum $ max 0 (t - now)
    !newremaining = f remaining
    !t'           = now + toEnum newremaining


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

    thr <- forkIOLabeledWithUnmaskBs "snap-server: timeout manager" $
             managerThread tm
    putMVar mthr thr
    return tm


------------------------------------------------------------------------------
-- | Stop a TimeoutManager.
stop :: TimeoutManager -> IO ()
stop tm = readMVar (_managerThread tm) >>= killThread


------------------------------------------------------------------------------
-- | Register a new connection with the TimeoutManager.
register :: IO ()
         -- ^ action to run when the timeout deadline is exceeded.
         -> TimeoutManager   -- ^ manager to register with.
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
cancel h = writeIORef (_state h) Canceled
{-# INLINE cancel #-}


------------------------------------------------------------------------------
managerThread :: TimeoutManager -> (forall a. IO a -> IO a) -> IO ()
managerThread tm unmask = unmask loop `finally` (readIORef connections >>= destroyAll)
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
