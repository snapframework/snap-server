{-# LANGUAGE BangPatterns #-}
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

import           Control.Applicative         ((<$>))
import           Control.Concurrent          (MVar, ThreadId, killThread, newEmptyMVar, putMVar, readMVar, tryReadMVar)
import           Control.Concurrent.Extended (forkIOLabeledWithUnmaskBs, forkOnLabeledWithUnmaskBs)
import qualified Control.Exception           as E
import           Control.Monad               (void)
import qualified Data.ByteString.Char8       as B
import           GHC.Exts                    (inline)


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

