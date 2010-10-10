{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Concurrent
import           Control.Exception (finally)

import           Snap.Http.Server
import           Test.Common.TestHandler


{-

/pong
/fileserve
/echo
pipelined POST requests
slowloris attack / timeout test

-}


main :: IO ()
main = do
    m <- newEmptyMVar

    forkIO $ go m
    takeMVar m

    return ()

  where
    go m = quickHttpServe testHandler `finally` putMVar m ()

