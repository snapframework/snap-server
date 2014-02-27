{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Snap.Internal.Http.Server.Common
  ( atomicModifyIORef'
  , eatException
  ) where

import           Control.Exception (SomeException, catch)
import           Control.Monad     (void)
import           Prelude           (IO, return, seq, ($!))

#if MIN_VERSION_base(4,6,0)
------------------------------------------------------------------------------
import           Data.IORef        (atomicModifyIORef')

#else
------------------------------------------------------------------------------
import           Data.IORef        (IORef, atomicModifyIORef)


------------------------------------------------------------------------------
-- | Strict version of 'atomicModifyIORef'.  This forces both the value stored
-- in the 'IORef' as well as the value returned.
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b
#endif


------------------------------------------------------------------------------
eatException :: IO a -> IO ()
eatException m = void m `catch` f
  where
    f :: SomeException -> IO ()
    f !_ = return $! ()
