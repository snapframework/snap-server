{-# LANGUAGE RankNTypes #-}

-- | Handy functions that should really be merged into
-- Control.Concurrent itself.
module Control.Concurrent.Extended
    ( forkIOLabeled
    , forkIOLabeledWithUnmask

    , forkOnLabeled
    , forkOnLabeledWithUnmask
    ) where

import Control.Exception
import Control.Concurrent
import GHC.Conc.Sync (labelThread)

-- | Sparks off a new thread using 'forkIO' to run the given IO
-- computation, but first labels the thread with the given label
-- (using 'labelThread').
--
-- The implementation makes sure that asynchronous exceptions are
-- masked until the given computation is executed. This ensures the
-- thread will always be labeled which guarantees you can always
-- easily find it in the GHC event log.
--
-- Note that the given computation is executed in the masked state of
-- the calling thread.
--
-- Returns the 'ThreadId' of the newly created thread.
forkIOLabeled :: String -- ^ Thread label
              -> IO ()
              -> IO ThreadId
forkIOLabeled label m =
    mask $ \restore -> forkIO $ do
      tid <- myThreadId
      labelThread tid label
      restore m

-- | Like 'forkIOLabeled', but lets you specify on which capability
-- (think CPU) the thread should run.
forkOnLabeled :: String -- ^ Thread label
              -> Int    -- ^ Capability
              -> IO ()
              -> IO ThreadId
forkOnLabeled label cap m =
    mask $ \restore -> forkOn cap $ do
      tid <- myThreadId
      labelThread tid label
      restore m

-- | Sparks off a new thread using 'forkIOWithUnmask' to run the given
-- IO computation, but first labels the thread with the given label
-- (using 'labelThread').
--
-- The implementation makes sure that asynchronous exceptions are
-- masked until the given computation is executed. This ensures the
-- thread will always be labeled which guarantees you can always
-- easily find it in the GHC event log.
--
-- Like 'forkIOWithUnmask', the given computation is given a function
-- to unmask asynchronous exceptions. See the documentation of that
-- function for the motivation.
--
-- Returns the 'ThreadId' of the newly created thread.
forkIOLabeledWithUnmask :: String -- ^ Thread label
                        -> ((forall a. IO a -> IO a) -> IO ())
                        -> IO ThreadId
forkIOLabeledWithUnmask label m =
    mask_ $ forkIOWithUnmask $ \unmask -> do
      tid <- myThreadId
      labelThread tid label
      m unmask

-- | Like 'forkIOLabeledWithUnmask', but lets you specify on which
-- capability (think CPU) the thread should run.
forkOnLabeledWithUnmask :: String -- ^ Thread label
                        -> Int    -- ^ Capability
                        -> ((forall a. IO a -> IO a) -> IO ())
                        -> IO ThreadId
forkOnLabeledWithUnmask label cap m =
    mask_ $ forkOnWithUnmask cap $ \unmask -> do
      tid <- myThreadId
      labelThread tid label
      m unmask
