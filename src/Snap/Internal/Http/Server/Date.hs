{-# LANGUAGE BangPatterns #-}

module Snap.Internal.Http.Server.Date
( getDateString
, getCurrentDateTime) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString as B
import           Data.IORef
import           Data.Time.Clock
import           Data.Time.Format
import           System.IO.Unsafe
import           System.Locale


-- Here comes a dirty hack. We don't want to be wasting context switches
-- building date strings, so we're only going to compute one every two
-- seconds. (Approximate timestamps to within a couple of seconds are OK here,
-- and we'll reduce overhead.)
--
-- Note that we also don't want to wake up a potentially sleeping CPU by just
-- running the computation on a timer. We'll allow client traffic to trigger
-- the process.

data DateState = DateState {
      _cachedDateString :: !(IORef ByteString)
    , _cachedDate       :: !(IORef UTCTime)
    , _valueIsOld       :: !(IORef Bool)
    , _morePlease       :: !(MVar ())
    , _dataAvailable    :: !(MVar ())
    , _dateThread       :: !(MVar ThreadId)
    }

dateState :: DateState
dateState = unsafePerformIO $ do
    (s,date) <- fetchTime
    bs <- newIORef s
    dt <- newIORef date
    ov <- newIORef False
    th <- newEmptyMVar
    mp <- newMVar ()
    da <- newMVar ()

    let d = DateState bs dt ov mp da th

    t  <- forkIO $ dateThread d
    putMVar th t

    return d


fetchTime :: IO (ByteString,UTCTime)
fetchTime = do
     now <- getCurrentTime
     return (B.pack $ map c2w $
             formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" now,
             now)


dateThread :: DateState -> IO ()
dateThread (DateState dateString time valueIsOld morePlease dataAvailable _) =
    forever $ do
        -- a lot of effort to make sure we don't deadlock

        takeMVar morePlease

        (s,now) <- fetchTime
        atomicModifyIORef dateString $ const (s,())
        atomicModifyIORef time $ const (now,())

        writeIORef valueIsOld False
        tryPutMVar dataAvailable ()

        threadDelay 2000000

        takeMVar dataAvailable
        writeIORef valueIsOld True


ensureFreshDate :: IO ()
ensureFreshDate = block $ do
    old <- readIORef $ _valueIsOld dateState
    when old $ do
        tryPutMVar (_morePlease dateState) ()
        readMVar $ _dataAvailable dateState

getDateString :: IO ByteString
getDateString = block $ do
    ensureFreshDate
    readIORef $ _cachedDateString dateState


getCurrentDateTime :: IO UTCTime
getCurrentDateTime = block $ do
    ensureFreshDate
    readIORef $ _cachedDate dateState

