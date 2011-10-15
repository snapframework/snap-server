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

import           Control.Monad
import           Control.Exception.Control
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
-- | Starts serving HTTP requests using the given handler. This function never
-- returns; to shut down the HTTP server, kill the controlling thread.
--
-- This function is like 'httpServe' except it doesn't setup compression or the
-- error handler; this allows it to be used from 'MonadSnap'.
simpleHttpServe :: MonadSnap m => Config m a -> Snap () -> IO ()
simpleHttpServe config handler = do 
    conf <- completeConfig config
    let output   = when (fromJust $ getVerbose conf) . hPutStrLn stderr
    mapM_ (output . ("Listening on "++) . show) $ listeners conf

    go conf `finally` output "\nShutting down..."
  where
    go conf = do
        let tout = fromMaybe 60 $ getDefaultTimeout conf
        setUnicodeLocale $ fromJust $ getLocale conf
        Int.httpServe tout
                      (listeners conf)
                      (fmap backendToInternal $ getBackend conf)
                      (fromJust $ getHostname  conf)
                      (fromJust $ getAccessLog conf)
                      (fromJust $ getErrorLog  conf)
                      (runSnap handler)
{-# INLINE simpleHttpServe #-}


listeners :: Config m a -> [Int.ListenPort]
listeners conf = catMaybes [ httpListener, httpsListener ]
  where
    httpsListener = do
        b    <- getSSLBind conf
        p    <- getSSLPort conf
        cert <- getSSLCert conf
        key  <- getSSLKey conf
        return $ Int.HttpsPort b p cert key

    httpListener = do
        p <- getPort conf
        b <- getBind conf
        return $ Int.HttpPort b p


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler, with settings from
-- the 'Config' passed in. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
httpServe :: Config Snap a -> Snap () -> IO ()
httpServe config handler = do
    conf <- completeConfig config
    let serve = compress conf . catch500 conf $ handler
    simpleHttpServe conf serve
{-# INLINE httpServe #-}


------------------------------------------------------------------------------
catch500 :: MonadSnap m => Config m a -> m () -> m ()
catch500 conf = flip catch $ fromJust $ getErrorHandler conf
{-# INLINE catch500 #-}


------------------------------------------------------------------------------
compress :: MonadSnap m => Config m a -> m () -> m ()
compress conf = if fromJust $ getCompression conf then withCompression else id
{-# INLINE compress #-}

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
setUnicodeLocale =
#ifndef PORTABLE
    \lang -> mapM_ (\k -> setEnv k (lang ++ ".UTF-8") True)
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
    const $ return ()
#endif


------------------------------------------------------------------------------
backendToInternal :: ConfigBackend -> Int.EventLoopType
backendToInternal ConfigSimpleBackend = Int.EventLoopSimple
backendToInternal ConfigLibEvBackend  = Int.EventLoopLibEv
