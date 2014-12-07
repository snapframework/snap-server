{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Snap.Internal.Http.Server.Clock
  ( ClockTime
  , getClockTime
  , sleepFor
  ) where

import           Control.Concurrent (threadDelay)
import           Data.Int           (Int64)
import           Data.Ratio         (Ratio, (%))
import qualified System.Clock       as Clock

type ClockTime = Double

------------------------------------------------------------------------------
_nano :: Int64
_nano = 1000000000

_micro :: Int64
_micro = 1000000

------------------------------------------------------------------------------
sleepFor :: ClockTime -> IO ()
sleepFor t = threadDelay $ truncate $ t0 * mc
  where
    t0 = (realToFrac t) :: Ratio Int64
    mc = _micro % 1

------------------------------------------------------------------------------
getClockTime :: IO ClockTime
getClockTime = do
    tm <- Clock.getTime Clock.Monotonic
    let !sec  = fromIntegral $ Clock.sec tm
    let !nsec = fromIntegral $ Clock.nsec tm
    let !out  = sec + (nsec % _nano)
    return $! realToFrac out
