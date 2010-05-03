-- | The Snap HTTP server is a high performance, epoll-enabled, iteratee-based
-- web server library written in Haskell. Together with the @snap-core@ library
-- upon which it depends, it provides a clean and efficient Haskell programming
-- interface to the HTTP protocol.
module Snap.Http.Server
(
  httpServe
) where

import           Data.ByteString (ByteString)
import           Snap.Types
import qualified Snap.Internal.Http.Server as Int


-- | Starts serving HTTP requests on the given port using the given handler.
httpServe :: ByteString      -- ^ bind address, or \"*\" for all
          -> Int             -- ^ port to bind to
          -> ByteString      -- ^ local hostname (server name)
          -> Maybe FilePath  -- ^ path to the (optional) access log
          -> Maybe FilePath  -- ^ path to the (optional) error log
          -> Snap ()         -- ^ handler procedure
          -> IO ()
httpServe bindAddress bindPort localHostname alog elog handler =
    Int.httpServe bindAddress bindPort localHostname alog elog handler'
  where
    handler' = runSnap handler
