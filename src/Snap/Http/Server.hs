-- | The Snap HTTP server is a high performance, epoll-enabled, iteratee-based
-- web server library written in Haskell. Together with the @snap-core@ library
-- upon which it depends, it provides a clean and efficient Haskell programming
-- interface to the HTTP protocol.
module Snap.Http.Server
(
  httpServe
, httpServeConfig
, quickHttpServe
, snapServerVersion
) where

import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import qualified Data.ByteString.Char8 as S
import           Data.ByteString (ByteString)
import           Snap.Types
import qualified Snap.Internal.Http.Server as Int
import           Snap.Http.Server.Config


------------------------------------------------------------------------------
-- | A short string describing the Snap server version
snapServerVersion :: ByteString
snapServerVersion = Int.snapServerVersion


------------------------------------------------------------------------------
-- | Starts serving HTTP requests on the given port using the given handler.
-- This function never returns; to shut down the HTTP server, kill the
-- controlling thread.
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


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler, with
-- settings from the 'Config' passed in.  This function will only
-- return after being interrupted by an asynchronous exception.
httpServeConfig :: Config -> Snap () -> IO ()
httpServeConfig conf handler = do
    ifNoisy . putStrLn $ "Listening on " ++
                         (S.unpack $ configBindAddress conf) ++
                         ":" ++ show (configListenPort conf)
    _ <- try serve :: IO (Either SomeException ())
    ifNoisy $ putStrLn " shutting down.."
  where
    ifNoisy = when $ configVerbose conf
    serve = httpServe (configBindAddress conf)
                      (configListenPort conf)
                      (configLocalHostname conf)
                      (configAccessLog conf)
                      (configErrorLog conf)
                      handler


------------------------------------------------------------------------------
-- | Starts serving HTTP using the given handler.  The configuration
-- is picked up from command-line parameters, as returned by
-- 'readConfigFromCmdLineArgs'.
quickHttpServe :: Snap () -> IO ()
quickHttpServe handler =
    readConfigFromCmdLineArgs >>= flip httpServeConfig handler
