{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Snap.Internal.Http.Server.Date
( getDateString
, getLogDateString
, getCurrentDateTime) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.IORef
import           Data.Maybe
import           Foreign.C.Types
import           System.IO.Unsafe
import           System.PosixCompat.Time

import           Snap.Internal.Http.Types (formatHttpTime, formatLogTime)

-- Here comes a dirty hack. We don't want to be wasting context switches
-- building date strings, so we're only going to compute one every second.
-- (Approximate timestamps to within a second are OK here, and we'll reduce
-- overhead.)
--
-- Note that we also don't want to wake up a potentially sleeping CPU by just
-- running the computation on a timer. We'll allow client traffic to trigger
-- the process.

------------------------------------------------------------------------------
data DateState = DateState {
      _cachedDateString :: !(IORef ByteString)
    , _cachedLogString  :: !(IORef ByteString)
    , _cachedDate       :: !(IORef CTime)
    , _valueIsOld       :: !(IORef Bool)
    , _clientActivity   :: !(IORef Bool)
    , _wakeUpThread     :: !(MVar ())
    , _dateThread       :: !(MVar ThreadId)
    }


------------------------------------------------------------------------------
dateState :: DateState
dateState = unsafePerformIO $ do
    (s1,s2,date) <- fetchTime
    bs1 <- newIORef s1
    bs2 <- newIORef s2
    dt  <- newIORef date
    ov  <- newIORef False
    act <- newIORef True
    th  <- newEmptyMVar
    wu  <- newMVar ()

    let d = DateState bs1 bs2 dt ov act wu th

    t  <- forkIO $ dateThread d
    putMVar th t

    return d


------------------------------------------------------------------------------
fetchTime :: IO (ByteString,ByteString,CTime)
fetchTime = do
    now <- epochTime
    t1  <- formatHttpTime now
    t2  <- formatLogTime now
    return (t1, t2, now)


------------------------------------------------------------------------------
updateState :: DateState -> IO ()
updateState (DateState dateString logString time valueIsOld activity _ _) = do
    (s1,s2,now) <- fetchTime
    atomicModifyIORef dateString $ const (s1,())
    atomicModifyIORef logString  $ const (s2,())
    atomicModifyIORef time       $ const (now,())
    writeIORef activity False
    writeIORef valueIsOld False

    -- force values in the iorefs to prevent thunk buildup
    !_ <- readIORef dateString
    !_ <- readIORef logString
    !_ <- readIORef time

    return ()


------------------------------------------------------------------------------
dateThread :: DateState -> IO ()
dateThread ds@(DateState _ _ _ valueIsOld activityRef wakeUpThread _) = loop
  where
    loop = do
        activity <- readIORef activityRef
        when (not activity) $ do
            writeIORef valueIsOld True
            takeMVar wakeUpThread

        updateState ds
        threadDelay 1000000
        loop


------------------------------------------------------------------------------
ensureFreshDate :: IO ()
ensureFreshDate = block $ do
    old <- readIORef $ _valueIsOld dateState
    if old 
      then do
        -- if the value is not fresh we will tickle the date thread but also fetch
        -- the new value immediately; we used to block but we'll do a little extra
        -- work to avoid a delay
        _ <- tryPutMVar (_wakeUpThread dateState) ()
        updateState dateState
      else writeIORef (_clientActivity dateState) True


------------------------------------------------------------------------------
getDateString :: IO ByteString
getDateString = block $ do
    ensureFreshDate
    readIORef $ _cachedDateString dateState


------------------------------------------------------------------------------
getLogDateString :: IO ByteString
getLogDateString = block $ do
    ensureFreshDate
    readIORef $ _cachedLogString dateState


------------------------------------------------------------------------------
getCurrentDateTime :: IO CTime
getCurrentDateTime = block $ do
    ensureFreshDate
    readIORef $ _cachedDate dateState
