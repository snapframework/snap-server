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
import           Data.Binary.Put
import           Data.Bits
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.DList (DList)
import qualified Data.DList as D
import           Data.Function
import           Data.Int
import           Data.IORef
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector as V
import           Data.Vector (Vector)
import qualified Data.Vector.Algorithms.Merge as VA
import           Data.Word
import           Foreign.C.Types (CTime)
import           GHC.Conc (numCapabilities)
import           Prelude hiding (catch, show)
import qualified Prelude
import           System.IO
import           Text.Show.ByteString

------------------------------------------------------------------------------
import           Data.Concurrent.HashMap (hashString, nextHighestPowerOf2)
import           Snap.Internal.Http.Server.Date


------------------------------------------------------------------------------
defaultNumberOfLocks :: Word
defaultNumberOfLocks = nextHighestPowerOf2 $ toEnum $ 4 * numCapabilities

------------------------------------------------------------------------------
hashToBucket :: Word -> Word
hashToBucket x = x .&. (defaultNumberOfLocks-1)


------------------------------------------------------------------------------
type Queue = DList (CTime, ByteString)

newtype MessageBuffer = MessageBuffer {
      _queues :: Vector (MVar Queue)
}

------------------------------------------------------------------------------
newMessageBuffer :: IO MessageBuffer
newMessageBuffer = liftM MessageBuffer $
                   V.replicateM (fromEnum defaultNumberOfLocks) (newMVar D.empty)


getAllMessages :: MessageBuffer -> IO (Vector ByteString)
getAllMessages (MessageBuffer queues) = do
    vec  <- liftM (V.concat . V.toList) $ V.mapM grabQ queues
    mvec <- V.unsafeThaw vec

    -- sort the list so the messages are emitted in time order
    VA.sortBy cmp mvec
    dvec <- V.unsafeFreeze mvec
    return $ V.map snd dvec

  where
    grabQ mv = modifyMVar mv $ \q -> return (D.empty, V.fromList $ D.toList q)
    cmp      = compare `on` fst


addMessageToQueue :: MVar Queue -> CTime -> ByteString -> IO ()
addMessageToQueue mv tm s = modifyMVar_ mv $ \q -> return $ D.snoc q (tm,s)


addMessage :: MessageBuffer -> CTime -> ByteString -> IO ()
addMessage (MessageBuffer queues) tm !s = do
    tid <- myThreadId
    let hash = hashString $ Prelude.show tid
    let bucket = hashToBucket hash
    let mv = V.unsafeIndex queues $ fromEnum bucket
    addMessageToQueue mv tm s


-- | Holds the state for a logger.
data Logger = Logger
    { _queuedMessages :: !MessageBuffer
    , _dataWaiting    :: !(MVar ())
    , _loggerPath     :: !(FilePath)
    , _loggingThread  :: !(MVar ThreadId) }


-- | Creates a new logger, logging to the given file. If the file argument is
-- \"-\", then log to stdout; if it's \"stderr\" then we log to stderr,
-- otherwise we log to a regular file in append mode. The file is closed and
-- re-opened every 15 minutes to facilitate external log rotation.
newLogger :: FilePath -> IO Logger
newLogger fp = do
    mb <- newMessageBuffer
    dw <- newEmptyMVar
    th <- newEmptyMVar

    let lg = Logger mb dw fp th

    tid <- forkIO $ loggingThread lg
    putMVar th tid

    return lg


-- | Prepares a log message with the time prepended.
timestampedLogEntry :: ByteString -> IO ByteString
timestampedLogEntry msg = do
    timeStr <- getLogDateString

    return $! S.concat $! L.toChunks $! runPut $! do
        putAscii '['
        putByteString timeStr
        putByteString "] "
        putByteString msg


-- | Prepares a log message in \"combined\" format.
combinedLogEntry :: ByteString        -- ^ remote host
                 -> Maybe ByteString  -- ^ remote user
                 -> ByteString        -- ^ request line (up to you to ensure
                                      --   there are no quotes in here)
                 -> Int               -- ^ status code
                 -> Maybe Int64       -- ^ num bytes sent
                 -> Maybe ByteString  -- ^ referer (up to you to ensure
                                      --   there are no quotes in here)
                 -> ByteString        -- ^ user agent (up to you to ensure
                                      --   there are no quotes in here)
                 -> IO ByteString
combinedLogEntry !host !mbUser !req !status !mbNumBytes !mbReferer !userAgent = do
    let user = fromMaybe "-" mbUser
    let referer = maybe "-" (\s -> S.concat ["\"", s, "\""]) mbReferer

    timeStr <- getLogDateString

    return $ S.concat $ L.toChunks $ runPut $ do
        putByteString host
        putByteString " - "
        putByteString user
        putByteString " ["
        putByteString timeStr
        putByteString "] \""
        putByteString req
        putByteString "\" "
        showp status
        putAscii ' '
        maybe (putAscii '-')
              (showp)
              mbNumBytes
        putAscii ' '
        putByteString referer
        putByteString " \""
        putByteString userAgent
        putAscii '\"'


-- | Sends out a log message verbatim with a newline appended. Note:
-- if you want a fancy log message you'll have to format it yourself
-- (or use 'combinedLogEntry').
logMsg :: Logger -> ByteString -> IO ()
logMsg !lg !s = do
    let !s' = S.snoc s '\n'
    tm <- getCurrentDateTime
    addMessage (_queuedMessages lg) tm s'
    tryPutMVar (_dataWaiting lg) () >> return ()


loggingThread :: Logger -> IO ()
loggingThread (Logger queue notifier filePath _) = do
    initialize >>= go

  where
    --------------------------------------------------------------------------
    openIt = if filePath == "-"
               then return stdout
               else if filePath == "stderr"
                      then return stderr
                      else do
                          h <- openFile filePath AppendMode
                          hSetBuffering h $ BlockBuffering $ Just 32768
                          return h

    --------------------------------------------------------------------------
    closeIt h = if filePath == "-" || filePath == "stderr"
                  then return ()
                  else hClose h

    --------------------------------------------------------------------------
    go (href, lastOpened) =
        (loop (href, lastOpened))
          `catches`
          [ Handler $ \(_::AsyncException) -> killit (href, lastOpened)
          , Handler $ \(e::SomeException)  -> do
                hPutStrLn stderr $ "logger got exception: " ++ Prelude.show e
                threadDelay 20000000
                go (href, lastOpened) ]

    --------------------------------------------------------------------------
    initialize = do
        lh   <- openIt
        href <- newIORef lh
        t    <- getCurrentDateTime
        tref <- newIORef t
        return (href, tref)

    --------------------------------------------------------------------------
    killit (href, lastOpened) = do
        flushIt (href, lastOpened)
        h <- readIORef href
        closeIt h

    --------------------------------------------------------------------------
    flushIt (!href, !lastOpened) = do
        msgs <- getAllMessages queue

        -- flush all messages out to buffer
        h <- readIORef href
        V.mapM_ (S.hPut h) msgs
        hFlush h

        -- close the file every 15 minutes (for log rotation)
        t <- getCurrentDateTime
        old <- readIORef lastOpened

        if t-old > 900
          then do
              closeIt h
              openIt >>= writeIORef href
              writeIORef lastOpened t
          else return ()


    loop !d = do
        -- wait on the notification mvar
        _ <- takeMVar notifier

        -- grab the queued messages and write them out
        flushIt d

        -- at least five seconds between log dumps
        threadDelay 5000000
        loop d


-- | Kills a logger thread, causing any unwritten contents to be
-- flushed out to disk
stopLogger :: Logger -> IO ()
stopLogger lg = withMVar (_loggingThread lg) killThread
