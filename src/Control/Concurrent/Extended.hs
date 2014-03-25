{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Handy functions that should really be merged into
-- Control.Concurrent itself.
module Control.Concurrent.Extended
    ( forkIOLabeledBs
    , forkIOLabeledWithUnmaskBs

    , forkOnLabeledBs
    , forkOnLabeledWithUnmaskBs

    , labelMe

    , labelThreadBs
    , labelThreadCString
    ) where

import Control.Exception
import Control.Concurrent
import Foreign.C.String (CString)
import GHC.Conc.Sync (ThreadId(..))
import GHC.Ptr (Ptr(..))
import GHC.IO (IO(..))
import GHC.Base (labelThread#)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as BU

-- | Sparks off a new thread using 'forkIO' to run the given IO
-- computation, but first labels the thread with the given label
-- (using 'labelThreadBs').
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
forkIOLabeledBs :: B.ByteString -- ^ Latin-1 encoded label
                -> IO ()
                -> IO ThreadId
forkIOLabeledBs label m =
    mask $ \restore -> forkIO $ do
      labelMe label
      restore m

-- | Like 'forkIOLabeledBs', but lets you specify on which capability
-- (think CPU) the thread should run.
forkOnLabeledBs :: B.ByteString -- ^ Latin-1 encoded label
                -> Int          -- ^ Capability
                -> IO ()
                -> IO ThreadId
forkOnLabeledBs label cap m =
    mask $ \restore -> forkOn cap $ do
      labelMe label
      restore m

-- | Sparks off a new thread using 'forkIOWithUnmask' to run the given
-- IO computation, but first labels the thread with the given label
-- (using 'labelThreadBs').
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
forkIOLabeledWithUnmaskBs :: B.ByteString -- ^ Latin-1 encoded label
                          -> ((forall a. IO a -> IO a) -> IO ())
                          -> IO ThreadId
forkIOLabeledWithUnmaskBs label m =
    mask_ $ forkIOWithUnmask $ \unmask -> do
      labelMe label
      m unmask

-- | Like 'forkIOLabeledWithUnmaskBs', but lets you specify on which
-- capability (think CPU) the thread should run.
forkOnLabeledWithUnmaskBs :: B.ByteString -- ^ Latin-1 encoded label
                          -> Int          -- ^ Capability
                          -> ((forall a. IO a -> IO a) -> IO ())
                          -> IO ThreadId
forkOnLabeledWithUnmaskBs label cap m =
    mask_ $ forkOnWithUnmask cap $ \unmask -> do
      labelMe label
      m unmask

-- | Label the current thread.
labelMe :: B.ByteString -> IO ()
#if defined(LABEL_THREADS)
labelMe label = do
    tid <- myThreadId
    labelThreadBs tid label
#else
labelMe _label = return ()
#endif
{-# INLINE labelMe #-}

-- | Like 'labelThread' but uses a Latin-1 encoded 'ByteString'
-- instead of a 'String'.
--
-- Note that if you terminate the ByteString with a '\0' this function
-- will use a more efficient implementation which avoids copying the
-- ByteString.
labelThreadBs :: ThreadId -> B.ByteString -> IO ()
labelThreadBs tid bs
    | n == 0                  = return ()
    | B.index bs (n - 1) == 0 = BU.unsafeUseAsCString bs $ labelThreadCString tid
    | otherwise               =        B.useAsCString bs $ labelThreadCString tid
  where
    n = B.length bs

-- | Like 'labelThread' but uses a 'CString' instead of a 'String'
labelThreadCString :: ThreadId -> CString -> IO ()
labelThreadCString (ThreadId t) (Ptr p) =
    IO $ \s -> case labelThread# t p s of
                 s1 -> (# s1, () #)
