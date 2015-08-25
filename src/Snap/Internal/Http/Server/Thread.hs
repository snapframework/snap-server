{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE RankNTypes   #-}

module Snap.Internal.Http.Server.Thread
  ( SnapThread
  , fork
  , forkOn
  , cancel
  , wait
  , cancelAndWait
  , isFinished
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative         ((<$>))
#endif
import           Control.Concurrent          (MVar, ThreadId, killThread, newEmptyMVar, putMVar, readMVar)
#if MIN_VERSION_base(4,7,0)
import           Control.Concurrent          (tryReadMVar)
#else
import           Control.Concurrent          (tryTakeMVar)
import           Control.Monad               (when)
import           Data.Maybe                  (fromJust, isJust)
#endif
import           Control.Concurrent.Extended (forkIOLabeledWithUnmaskBs, forkOnLabeledWithUnmaskBs)
import qualified Control.Exception           as E
import           Control.Monad               (void)
import qualified Data.ByteString.Char8       as B
import           GHC.Exts                    (inline)

#if !MIN_VERSION_base(4,7,0)
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar mv = do
    m <- tryTakeMVar mv
    when (isJust m) $ putMVar mv (fromJust m)
    return m
#endif

------------------------------------------------------------------------------
data SnapThread = SnapThread {
      _snapThreadId :: {-# UNPACK #-} !ThreadId
    , _snapThreadFinished :: {-# UNPACK #-} !(MVar ())
    }

instance Show SnapThread where
  show = show . _snapThreadId


------------------------------------------------------------------------------
forkOn :: B.ByteString                          -- ^ thread label
       -> Int                                   -- ^ capability
       -> ((forall a . IO a -> IO a) -> IO ())  -- ^ user thread action, taking
                                                --   a restore function
       -> IO SnapThread
forkOn label cap action = do
    mv <- newEmptyMVar
    E.uninterruptibleMask_ $ do
        tid <- forkOnLabeledWithUnmaskBs label cap (wrapAction mv action)
        return $! SnapThread tid mv


------------------------------------------------------------------------------
fork :: B.ByteString                          -- ^ thread label
     -> ((forall a . IO a -> IO a) -> IO ())  -- ^ user thread action, taking
                                              --   a restore function
     -> IO SnapThread
fork label action = do
    mv <- newEmptyMVar
    E.uninterruptibleMask_ $ do
        tid <- forkIOLabeledWithUnmaskBs label (wrapAction mv action)
        return $! SnapThread tid mv


------------------------------------------------------------------------------
cancel :: SnapThread -> IO ()
cancel = killThread . _snapThreadId


------------------------------------------------------------------------------
wait :: SnapThread -> IO ()
wait = void . readMVar . _snapThreadFinished


------------------------------------------------------------------------------
cancelAndWait :: SnapThread -> IO ()
cancelAndWait t = cancel t >> wait t


------------------------------------------------------------------------------
isFinished :: SnapThread -> IO Bool
isFinished t =
    maybe False (const True) <$> tryReadMVar (_snapThreadFinished t)


------------------------------------------------------------------------------
-- Internal functions follow
------------------------------------------------------------------------------
wrapAction :: MVar ()
           -> ((forall a . IO a -> IO a) -> IO ())
           -> ((forall a . IO a -> IO a) -> IO ())
wrapAction mv action restore = (action restore >> inline exit) `E.catch` onEx
  where
    onEx :: E.SomeException -> IO ()
    onEx !_ = inline exit

    exit = E.uninterruptibleMask_ (putMVar mv $! ())
