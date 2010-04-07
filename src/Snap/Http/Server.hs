module Snap.Http.Server
(
  httpServe
) where

import           Data.ByteString (ByteString)
import           Snap.Types
import qualified Snap.Internal.Http.Server as Int


-- | FIXME: document this
httpServe :: ByteString      -- ^ bind address, or \"*\" for all
          -> Int             -- ^ port to bind to
          -> ByteString      -- ^ local hostname (server name)
          -> Maybe FilePath  -- ^ path to the access log
          -> Maybe FilePath  -- ^ path to the error log
          -> Snap ()         -- ^ handler procedure
          -> IO ()
httpServe bindAddress bindPort localHostname alog elog handler =
    Int.httpServe bindAddress bindPort localHostname alog elog handler'
  where
    handler' = runSnap handler
