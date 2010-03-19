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
          -> Snap ()         -- ^ handler procedure
          -> IO ()
httpServe bindAddress bindPort localHostname handler =
    Int.httpServe bindAddress bindPort localHostname handler'
  where
    handler' = runSnap handler
        
