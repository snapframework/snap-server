{-# LANGUAGE BangPatterns #-}

module Snap.Internal.Http.Server.Common (eatException) where

import           Control.Exception (SomeException, catch)
import           Control.Monad     (void)
import           Prelude           (IO, return, ($!))

------------------------------------------------------------------------------
eatException :: IO a -> IO ()
eatException m = void m `catch` f
  where
    f :: SomeException -> IO ()
    f !_ = return $! ()
