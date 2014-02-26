{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.FastLogger
  ( Logger
  , timestampedLogEntry
  , combinedLogEntry
  , newLogger
  , newLoggerWithCustomErrorFunction
  , withLogger
  , withLoggerWithCustomErrorFunction
  , stopLogger
  , logMsg
  ) where


------------------------------------------------------------------------------
import           Blaze.ByteString.Builder           (Builder, fromByteString,
                                                     fromWord8, toByteString,
                                                     toLazyByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import           Control.Concurrent                 (MVar, ThreadId, forkIO,
                                                     killThread, newEmptyMVar,
                                                     putMVar, takeMVar,
                                                     threadDelay, tryPutMVar,
                                                     withMVar)
import           Control.Exception                  (AsyncException,
                                                     Handler (..),
                                                     SomeException, bracket,
                                                     catch, catches)
import           Control.Monad                      (unless, void, when)
import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as S
import           Data.ByteString.Internal           (c2w)
import qualified Data.ByteString.Lazy.Char8         as L
import           Data.Int                           (Int64)
import           Data.IORef                         (IORef, atomicModifyIORef,
                                                     newIORef, readIORef,
                                                     writeIORef)
import           Data.Monoid                        (mappend, mconcat, mempty)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
#if !MIN_VERSION_base(4,6,0)
import           Prelude                            hiding (catch)
#endif
import           System.IO                          (IOMode (AppendMode),
                                                     hClose, hFlush, openFile,
                                                     stderr, stdout)

import           Snap.Internal.Http.Server.Date     (getCurrentDateTime,
                                                     getLogDateString)


------------------------------------------------------------------------------
-- | Holds the state for a logger.
data Logger = Logger
    { _queuedMessages :: !(IORef Builder)
    , _dataWaiting    :: !(MVar ())
    , _loggerPath     :: !(FilePath)
    , _loggingThread  :: !(MVar ThreadId)
    , _errAction      :: ByteString -> IO ()
    }


------------------------------------------------------------------------------
-- | Creates a new logger, logging to the given file. If the file argument is
-- \"-\", then log to stdout; if it's \"stderr\" then we log to stderr,
-- otherwise we log to a regular file in append mode. The file is closed and
-- re-opened every 15 minutes to facilitate external log rotation.
newLogger :: FilePath                      -- ^ log file to use
          -> IO Logger
newLogger = newLoggerWithCustomErrorFunction
              (\s -> S.hPutStr stderr s >> hFlush stderr)


------------------------------------------------------------------------------
-- | Like 'newLogger', but uses a custom error action if the logger needs to
-- print an error message of its own (for instance, if it can't open the
-- output file.)
newLoggerWithCustomErrorFunction :: (ByteString -> IO ())
                                     -- ^ logger uses this action to log any
                                     -- error messages of its own
                                 -> FilePath   -- ^ log file to use
                                 -> IO Logger
newLoggerWithCustomErrorFunction errAction fp = do
    q  <- newIORef mempty
    dw <- newEmptyMVar
    th <- newEmptyMVar

    let lg = Logger q dw fp th errAction

    tid <- forkIO $ loggingThread lg
    putMVar th tid

    return lg


------------------------------------------------------------------------------
-- | Creates a Logger and passes it into the given function, cleaning up
-- with \"stopLogger\" afterwards.
withLogger :: FilePath                      -- ^ log file to use
          -> (Logger -> IO a)
          -> IO a
withLogger f = bracket (newLogger f) stopLogger


------------------------------------------------------------------------------
-- | Creates a Logger with \"newLoggerWithCustomErrorFunction\" and passes it
-- into the given function, cleaning up with \"stopLogger\" afterwards.
withLoggerWithCustomErrorFunction :: (ByteString -> IO ())
                                     -- ^ logger uses this action to log any
                                     -- error messages of its own
                                  -> FilePath       -- ^ log file to use
                                  -> (Logger -> IO a)
                                  -> IO a
withLoggerWithCustomErrorFunction e f =
    bracket (newLoggerWithCustomErrorFunction e f) stopLogger


------------------------------------------------------------------------------
-- FIXME: can be a builder, and we could even use the same trick we use for
-- HTTP
--
-- | Prepares a log message with the time prepended.
timestampedLogEntry :: ByteString -> IO ByteString
timestampedLogEntry msg = do
    timeStr <- getLogDateString

    return $! toByteString
           $! mconcat [ fromWord8 $ c2w '['
                      , fromByteString timeStr
                      , fromByteString "] "
                      , fromByteString msg ]


------------------------------------------------------------------------------
-- FIXME: builder
--
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
combinedLogEntry !host !mbUser !req !status !mbNumBytes !mbReferer !ua = do
    timeStr <- getLogDateString

    let !l = [ fromByteString host
             , fromByteString " - "
             , user
             , fromByteString " ["
             , fromByteString timeStr
             , fromByteString "] \""
             , fromByteString req
             , fromByteString "\" "
             , fromShow status
             , space
             , numBytes
             , space
             , referer
             , fromByteString " \""
             , fromByteString ua
             , quote ]

    let !output = toByteString $ mconcat l

    return $! output

  where
    dash     = fromWord8 $ c2w '-'
    quote    = fromWord8 $ c2w '\"'
    space    = fromWord8 $ c2w ' '
    user     = maybe dash fromByteString mbUser
    numBytes = maybe dash fromShow mbNumBytes
    referer  = maybe dash
                     (\s -> mconcat [ quote
                                    , fromByteString s
                                    , quote ])
                     mbReferer


------------------------------------------------------------------------------
-- | Sends out a log message verbatim with a newline appended. Note:
-- if you want a fancy log message you'll have to format it yourself
-- (or use 'combinedLogEntry').
logMsg :: Logger -> ByteString -> IO ()
logMsg !lg !s = do
    let !s' = fromByteString s `mappend` fromWord8 (c2w '\n')
    atomicModifyIORef (_queuedMessages lg) $ \d -> (d `mappend` s',())
    void $ tryPutMVar (_dataWaiting lg) ()


------------------------------------------------------------------------------
loggingThread :: Logger -> IO ()
loggingThread (Logger queue notifier filePath _ errAct) = initialize >>= go
  where
    --------------------------------------------------------------------------
    openIt | filePath == "-"      = return stdout
           | filePath == "stderr" = return stderr
           | otherwise =
               openFile filePath AppendMode `catch` \(e::SomeException) -> do
                   logInternalError $ "Can't open log file \"" ++
                                      filePath ++ "\".\n"
                   logInternalError $ "Exception: " ++ show e ++ "\n"
                   logInternalError $ "Logging to stderr instead. " ++
                                      "**THIS IS BAD, YOU OUGHT TO " ++
                                      "FIX THIS**\n\n"
                   return stderr

    --------------------------------------------------------------------------
    closeIt h = unless (h == stdout || h == stderr) $ hClose h

    --------------------------------------------------------------------------
    logInternalError = errAct . T.encodeUtf8 . T.pack

    --------------------------------------------------------------------------
    go (href, lastOpened) =
        loop (href, lastOpened) `catches`
          [ Handler $ \(_::AsyncException) -> killit (href, lastOpened)
          , Handler $ \(e::SomeException)  -> do
                logInternalError $ "logger got exception: "
                                   ++ Prelude.show e ++ "\n"
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
        dl <- atomicModifyIORef queue $ \x -> (mempty,x)

        let !msgs = toLazyByteString dl
        h <- readIORef href
        (do L.hPut h msgs
            hFlush h) `catch` \(e::SomeException) -> do
                logInternalError $ "got exception writing to log " ++
                                   filePath ++ ": " ++ show e ++ "\n"
                logInternalError "writing log entries to stderr.\n"
                mapM_ errAct $ L.toChunks msgs

        -- close the file every 15 minutes (for log rotation)
        t   <- getCurrentDateTime
        old <- readIORef lastOpened

        when (t - old > 900) $ do
            closeIt h
            openIt >>= writeIORef href
            writeIORef lastOpened t

    --------------------------------------------------------------------------
    loop !d = do
        -- wait on the notification mvar
        _ <- takeMVar notifier

        -- grab the queued messages and write them out
        flushIt d

        -- at least five seconds between log dumps
        threadDelay 5000000
        loop d


------------------------------------------------------------------------------
-- | Kills a logger thread, causing any unwritten contents to be
-- flushed out to disk
stopLogger :: Logger -> IO ()
stopLogger lg = withMVar (_loggingThread lg) killThread
