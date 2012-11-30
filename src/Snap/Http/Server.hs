{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | The Snap HTTP server is a high performance web server library written in
-- Haskell. Together with the @snap-core@ library upon which it depends, it
-- provides a clean and efficient Haskell programming interface to the HTTP
-- protocol.
--
module Snap.Http.Server
  ( simpleHttpServe
  , httpServe
  , quickHttpServe
  , snapServerVersion
  , setUnicodeLocale
  , module Snap.Http.Server.Config
  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8   (ByteString)
#ifndef PORTABLE
import           System.Posix.Env
#endif
------------------------------------------------------------------------------
import           Snap.Core               (MonadSnap (..), Snap)
import           Snap.Http.Server.Config


------------------------------------------------------------------------------
-- | A short string describing the Snap server version
snapServerVersion :: ByteString
snapServerVersion = undefined


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler. This function never
-- returns; to shut down the HTTP server, kill the controlling thread.
--
-- This function is like 'httpServe' except it doesn't setup compression,
-- reverse proxy address translation (via 'Snap.Util.Proxy.behindProxy'), or
-- the error handler; this allows it to be used from 'MonadSnap'.
simpleHttpServe :: MonadSnap m => Config m a -> Snap () -> IO ()
simpleHttpServe = undefined


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler, with settings from
-- the 'Config' passed in. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
httpServe :: Config Snap a -> Snap () -> IO ()
httpServe = undefined


------------------------------------------------------------------------------
-- | Starts serving HTTP using the given handler. The configuration is read
-- from the options given on the command-line, as returned by
-- 'commandLineConfig'. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
quickHttpServe :: Snap () -> IO ()
quickHttpServe = undefined


------------------------------------------------------------------------------
-- | Given a string like \"en_US\", this sets the locale to \"en_US.UTF-8\".
-- This doesn't work on Windows.
setUnicodeLocale :: String -> IO ()
#ifndef PORTABLE
setUnicodeLocale lang = mapM_ (\k -> setEnv k (lang ++ ".UTF-8") True)
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
setUnicodeLocale = const $ return ()
#endif
