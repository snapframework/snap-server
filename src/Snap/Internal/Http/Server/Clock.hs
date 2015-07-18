{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Snap.Internal.Http.Server.Clock
  ( ClockTime
  , getClockTime
  , sleepFor
  , sleepSecs
  , fromSecs
  , toSecs
  ) where

import           Control.Concurrent (threadDelay)
import qualified System.Clock       as Clock

type ClockTime = Clock.TimeSpec

------------------------------------------------------------------------------
sleepFor :: ClockTime -> IO ()
sleepFor t = threadDelay $ fromIntegral d
  where
    d  = (Clock.nsec t `div` 1000) + (1000000 * Clock.sec t)


------------------------------------------------------------------------------
sleepSecs :: Double -> IO ()
sleepSecs = sleepFor . fromSecs


------------------------------------------------------------------------------
getClockTime :: IO ClockTime
getClockTime = Clock.getTime Clock.Monotonic


------------------------------------------------------------------------------
fromSecs :: Double -> ClockTime
fromSecs d = let (s, r) = properFraction d
             in Clock.TimeSpec s (truncate $! 1000000000 * r)


------------------------------------------------------------------------------
toSecs :: ClockTime -> Double
toSecs t = fromIntegral (Clock.sec t) +
           fromIntegral (Clock.nsec t) / 1000000000.0
