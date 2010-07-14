{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

The Snap HTTP server is a high performance, epoll-enabled, iteratee-based web
server library written in Haskell. Together with the @snap-core@ library upon
which it depends, it provides a clean and efficient Haskell programming
interface to the HTTP protocol.

-}

module Snap.Http.Server
  ( simpleHttpServe
  , httpServe
  , quickHttpServe
  , snapServerVersion
  , setUnicodeLocale
  , module Snap.Http.Server.Config
  ) where

import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import qualified Data.ByteString.UTF8 as U
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude hiding (catch)
import           Snap.Http.Server.Config
import qualified Snap.Internal.Http.Server as Int
import           Snap.Types
import           Snap.Util.GZip
#ifndef PORTABLE
import           System.Posix.Env
#endif
import           System.IO


------------------------------------------------------------------------------
-- | A short string describing the Snap server version
snapServerVersion :: ByteString
snapServerVersion = Int.snapServerVersion


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler. Uses only the basic
-- settings from the given config; error handling and compression are ignored.
-- This function never returns; to shut down the HTTP server, kill the
-- controlling thread.
simpleHttpServe :: MonadSnap m => Config m a -> Snap () -> IO ()
simpleHttpServe config handler = do
    setUnicodeLocale $ fromJust $ getLocale conf
    Int.httpServe (fromJust $ getAddress   conf)
                  (fromJust $ getPort      conf)
                  (fromJust $ getHostname  conf)
                  (fromJust $ getAccessLog conf)
                  (fromJust $ getErrorLog  conf)
                  (runSnap handler)
  where
    conf = completeConfig config


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler, with settings from
-- the 'Config' passed in. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
httpServe :: Config Snap () -> Snap () -> IO ()
httpServe config handler = do
    output $ "Listening on " ++ (U.toString $ fromJust $ getAddress conf) ++
        ":" ++ (show $ fromJust $ getPort conf)
    _ <- try $ serve handler :: IO (Either SomeException ())
    output "\nShutting down..."
  where
    conf     = completeConfig config
    output   = when (fromJust $ getVerbose conf) . hPutStrLn stderr
    serve    = simpleHttpServe config . compress . catch500
    catch500 = flip catch $ fromJust $ getErrorHandler conf
    compress = if fromJust $ getCompression conf then withCompression else id


------------------------------------------------------------------------------
-- | Starts serving HTTP using the given handler. The configuration is read
-- from the options given on the command-line, as returned by
-- 'commandLineConfig'. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
quickHttpServe :: Snap () -> IO ()
quickHttpServe m = commandLineConfig emptyConfig >>= \c -> httpServe c m


------------------------------------------------------------------------------
-- | Given a string like \"en_US\", this sets the locale to \"en_US.UTF-8\".
-- This doesn't work on Windows.
setUnicodeLocale :: String -> IO ()
setUnicodeLocale lang =
#ifndef PORTABLE
    mapM_ (\k -> setEnv k (lang ++ ".UTF-8") True)
          [ "LANG"
          , "LC_CTYPE"
          , "LC_NUMERIC"
          , "LC_TIME"
          , "LC_COLLATE"
          , "LC_MONETARY"
          , "LC_MESSAGES"
          , "LC_PAPER"
          , "LC_NAME"
          , "LC_ADDRESS"
          , "LC_TELEPHONE"
          , "LC_MEASUREMENT"
          , "LC_IDENTIFICATION"
          , "LC_ALL" ]
#else
    return ()
#endif
