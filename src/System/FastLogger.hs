{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.FastLogger 
( Logger
, timestampedLogEntry
, combinedLogEntry
, newLogger
, logMsg
, stopLogger
) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Array
import           Data.Binary.Put
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Internal (c2w)
import           Data.DList (DList)
import qualified Data.DList as D
import           Data.IORef
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Prelude hiding (catch)
import           System.IO
import           Text.Show.ByteString hiding (show)

-- | Holds the state for a logger.
data Logger = Logger
    { _queuedMessages :: !(IORef (DList ByteString))
    , _dataWaiting    :: !(MVar ())
    , _loggerPath     :: !(FilePath)
    , _loggingThread  :: !(MVar ThreadId) }


-- | Create a new logger, logging to the given file. If the file argument is
-- \"-\", then log to stdout; if it's \"stderr\" then we log to stderr,
-- otherwise we log to a regular file in append mode. The file is closed and
-- re-opened every 15 minutes to facilitate external log rotation.
newLogger :: FilePath -> IO Logger
newLogger fp = do
    q  <- newIORef D.empty
    dw <- newEmptyMVar
    th <- newEmptyMVar

    let lg = Logger q dw fp th

    tid <- forkIO $ loggingThread lg
    putMVar th tid

    return lg

-- | Prepare a log message with the time prepended.
timestampedLogEntry :: ByteString -> IO ByteString
timestampedLogEntry msg = do
    -- FIXME: replace haskell time functions with something quicker if
    -- available (e.g. @ev\_now()@ from libev which doesn't involve a context
    -- switch)
    zt <- getZonedTime

    let t = zonedTimeToLocalTime zt
    let (yr,mon,day) = toGregorian $ localDay t
    let tod = localTimeOfDay t

    let h = todHour tod
    let m = todMin  tod
    let s = (round $ todSec tod) :: Int

    let p = do
        showp yr
        putCh '-'
        when (mon < 10) $ putCh '0'
        showp mon
        putCh '-'
        when (day < 10) $ putCh '0'
        showp day
        putCh ' '
        when (h < 10) $ putCh '0'
        showp h
        putCh ':'
        when (m < 10) $ putCh '0'
        showp m
        putCh ':'
        when (s < 10) $ putCh '0'
        showp s
        putByteString ": "
        putByteString msg

    return $ B.concat $ L.toChunks (runPut p)

  where
    putCh = putWord8 . c2w

-- | Prepare a log message in \"combined\" format.
combinedLogEntry :: ByteString        -- ^ remote host
                 -> Maybe ByteString  -- ^ remote user
                 -> ByteString        -- ^ request line (up to you to ensure
                                      --   there are no quotes in here)
                 -> Int               -- ^ status code
                 -> Maybe Int         -- ^ num bytes sent
                 -> Maybe ByteString  -- ^ referer (up to you to ensure
                                      --   there are no quotes in here)
                 -> ByteString        -- ^ user agent (up to you to ensure
                                      --   there are no quotes in here)
                 -> IO L.ByteString
combinedLogEntry host mbUser req status mbNumBytes mbReferer userAgent = do
    let user = fromMaybe "-" mbUser
    let numBytes = maybe "-" (\s -> B.pack $ show s) mbNumBytes
    let referer = maybe "-" (\s -> B.concat ["\"", s, "\""]) mbReferer

    -- FIXME: replace haskell time functions with something quicker if
    -- available (e.g. @ev\_now()@ from libev which doesn't involve a context
    -- switch)
    zt <- getZonedTime

    let t = zonedTimeToLocalTime zt
    let zone = timeZoneOffsetString $ zonedTimeZone zt

    let (yr,mon,day) = toGregorian $ localDay t
    let tod = localTimeOfDay t

    let h = todHour tod
    let m = todMin  tod
    let s = (round $ todSec tod) :: Int

    let p = do
        putByteString host
        putByteString " - "
        putByteString user
        putByteString " ["
        when (day < 10) $ putCh '0'
        showp day
        putCh '/'
        putByteString $ month mon
        putCh '/'
        showp yr
        putCh ':'
        when (h < 10) $ putCh '0'
        showp h
        putCh ':'
        when (m < 10) $ putCh '0'
        showp m
        putCh ':'
        when (s < 10) $ putCh '0'
        showp s
        putCh ' '
        putByteString $ B.pack zone
        putByteString "] \""
        putByteString req
        putByteString "\" "
        showp status
        putCh ' '
        putByteString numBytes
        putCh ' '
        putByteString referer
        putByteString " \""
        putByteString userAgent
        putCh '\"'

    return $ runPut p


  where
    putCh = putWord8 . c2w
    monthArray = array (1,12) [ (1, "Jan")
                              , (2, "Feb")
                              , (3, "Mar")
                              , (4, "Apr")
                              , (5, "May")
                              , (6, "Jun")
                              , (7, "Jul")
                              , (8, "Aug")
                              , (9, "Sep")
                              , (10,"Oct")
                              , (11,"Nov")
                              , (12,"Dec") ]

    month i = monthArray ! i

-- | Send a log message out verbatim with a newline appended. Note: if you want
-- a fancy log message you'll have to format it yourself (or use
-- 'combinedLogEntry').
logMsg :: Logger -> ByteString -> IO ()
logMsg lg s = do
    let s' = B.snoc s '\n'
    atomicModifyIORef (_queuedMessages lg) $ \d -> (D.snoc d s',())
    tryPutMVar (_dataWaiting lg) () >> return ()


loggingThread :: Logger -> IO ()
loggingThread (Logger queue notifier filePath _) = do
    initialize >>= go

  where
    openIt = if filePath == "-"
               then return stdout
               else if filePath == "stderr"
                      then return stderr
                      else openFile filePath AppendMode

    closeIt h = if filePath == "-" || filePath == "stderr"
                  then return ()
                  else hClose h

    go (href, lastOpened) =
        (loop (href, lastOpened))
          `catches`
          [ Handler $ \(_::AsyncException) -> killit (href, lastOpened)
          , Handler $ \(e::SomeException)  -> do
                hPutStrLn stderr $ "logger got exception: " ++ show e
                threadDelay 20000000
                go (href, lastOpened) ]


    initialize = do
        lh   <- openIt
        href <- newIORef lh
        t    <- getCurrentTime
        tref <- newIORef t
        return (href, tref)


    killit (href, lastOpened) = do
        flushIt (href, lastOpened)
        h <- readIORef href
        closeIt h


    flushIt (href, lastOpened) = do
        dl <- atomicModifyIORef queue $ \x -> (D.empty,x)

        let msgs = D.toList dl
        let s = L.fromChunks msgs
        h <- readIORef href
        L.hPut h s
        hFlush h

        -- close the file every 15 minutes (for log rotation)
        t <- getCurrentTime
        old <- readIORef lastOpened

        if diffUTCTime t old > 900
          then do
              closeIt h
              openIt >>= writeIORef href
              writeIORef lastOpened t
          else return ()


    loop (href, lastOpened) = do
        -- wait on the notification mvar
        _ <- takeMVar notifier

        -- grab the queued messages and write them out
        flushIt (href, lastOpened)

        -- at least five seconds between log dumps
        threadDelay 5000000

        loop (href, lastOpened)


-- | Kill a logger thread, causing any unwritten contents to be flushed out to
-- disk
stopLogger :: Logger -> IO ()
stopLogger lg = withMVar (_loggingThread lg) killThread
