{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Snap.Internal.Http.Server.Date
( getDateString
, getLogDateString
, getCurrentDateTime) where

import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.IORef
import           Foreign.C.Types
import           System.IO.Unsafe
import           System.PosixCompat.Time

import           Snap.Internal.Http.Types (formatHttpTime, formatLogTime)

------------------------------------------------------------------------------
data DateState = DateState {
      _cachedDateString :: !(IORef ByteString)
    , _cachedLogString  :: !(IORef ByteString)
    , _lastFetchTime    :: !(IORef CTime)
    }


------------------------------------------------------------------------------
dateState :: DateState
dateState = unsafePerformIO $ do
    (s1,s2,date) <- fetchTime
    bs1 <- newIORef s1
    bs2 <- newIORef s2
    dt  <- newIORef date

    return $! DateState bs1 bs2 dt


------------------------------------------------------------------------------
fetchTime :: IO (ByteString,ByteString,CTime)
fetchTime = do
    now <- epochTime
    t1  <- formatHttpTime now
    t2  <- formatLogTime now
    return (t1, t2, now)


------------------------------------------------------------------------------
updateState :: DateState -> IO ()
updateState (DateState dateString logString time) = do
    (s1,s2,now) <- fetchTime
    atomicModifyIORef dateString $ const (s1,())
    atomicModifyIORef logString  $ const (s2,())
    atomicModifyIORef time       $ const (now,())

    -- force values in the iorefs to prevent thunk buildup
    !_ <- readIORef dateString
    !_ <- readIORef logString
    !_ <- readIORef time

    return ()


------------------------------------------------------------------------------
ensureFreshDate :: IO ()
ensureFreshDate = block $ do
    now <- epochTime
    old <- readIORef $ _lastFetchTime dateState
    when (now > old) $ updateState dateState


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
getCurrentDateTime = epochTime
