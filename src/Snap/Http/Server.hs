{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , rawHttpServe
  , module Snap.Http.Server.Config
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent                      (killThread, newEmptyMVar, putMVar, readMVar)
import           Control.Concurrent.Extended             (forkIOLabeledWithUnmaskBs)
import           Control.Exception                       (finally)
import qualified Control.Exception.Lifted                as E
import           Control.Monad                           (forM, when)
import           Data.ByteString.Char8                   (ByteString)
import qualified Data.ByteString.Char8                   as S
import           Data.Maybe                              (fromJust)
import           Data.Version                            (showVersion)
import           Prelude                                 (Bool (..), IO, Monad (..), String, flip, id, mapM_, maybe, null, ($), ($!), (++), (.))
#ifndef PORTABLE
import           System.Posix.Env
#endif
------------------------------------------------------------------------------
import qualified Paths_snap_server                       as V
import           Snap.Core                               (MonadSnap (..), Snap)
import           Snap.Http.Server.CmdlineConfig          (listeners, toServerConfig)
import           Snap.Http.Server.Config
import           Snap.Internal.Http.Server.Cleanup       (Cleanup)
import qualified Snap.Internal.Http.Server.Cleanup       as Cleanup
import           Snap.Internal.Http.Server.CmdlineConfig (CmdlineConfig, ProxyType (..), cmdlineConfig, defaultCmdlineConfig, getCompression, getErrorHandler, getLocale, getProxyType)
import           Snap.Internal.Http.Server.Session       (httpAcceptLoop, snapToServerHandler)
import           Snap.Internal.Http.Server.Types         (Backend (..))
import           Snap.Util.GZip                          (withCompression)
import           Snap.Util.Proxy                         (behindProxy)
import qualified Snap.Util.Proxy                         as Proxy
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | A short string describing the Snap server version
snapServerVersion :: ByteString
snapServerVersion = S.pack $! showVersion V.version


------------------------------------------------------------------------------
rawHttpServe :: ServerHandler s                 -- ^ server handler
             -> [Backend s]                     -- ^ backends
             -> Cleanup ()
rawHttpServe h backends = mapM_ spawnLoop backends
  where
    spawnLoop backend =
        Cleanup.cleanup
           (do let cfg  = _backendServerConfig backend
               let loop = _backendAcceptFunc backend
               let desc = _backendBindAddress backend
               mv <- newEmptyMVar
               tid <- forkIOLabeledWithUnmaskBs
                        (S.append "snap-server http master thread for " desc) $
                        \r -> (r $ httpAcceptLoop h cfg loop)
                                  `finally` putMVar mv ()
               return (mv, tid))
           (\(mv, tid) -> killThread tid `finally` readMVar mv)


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler. This function never
-- returns; to shut down the HTTP server, kill the controlling thread.
--
-- This function is like 'httpServe' except it doesn't setup compression,
-- reverse proxy address translation (via 'Snap.Util.Proxy.behindProxy'), or
-- the error handler; this allows it to be used from 'MonadSnap'.
simpleHttpServe :: MonadSnap m
                => ServerConfig hookState
                -> CmdlineConfig m a
                -> Snap ()
                -> IO ()
simpleHttpServe defaultServerConfig cmdline handler = do
    maybe (return $! ()) setUnicodeLocale $ getLocale cmdline
    Cleanup.runCleanup $ do
        (scfg, vlog) <- toServerConfig defaultServerConfig cmdline
        backends <- forM (listeners cmdline) $ \(name, start, secure) ->
            startBackend vlog scfg name secure start
        -- TODO: throw a proper exception here.
        when (null backends) $ fail "No backends configured."
        let shandler = snapToServerHandler handler
        rawHttpServe shandler backends
  where
{-# INLINE simpleHttpServe #-}


------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler, with settings from
-- the 'CmdlineConfig' passed in. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
httpServe :: CmdlineConfig Snap a -> Snap () -> IO ()
httpServe cmdline handler0 = do
    let !handler = chooseProxy cmdline
    -- TODO: refactor handler wrapping into separate function of type
    -- "CmdlineConfig m a -> m a -> m a"
    let serve    = compress cmdline . catch500 cmdline $ handler
    simpleHttpServe emptyServerConfig cmdline serve

  where
    -- TODO: refactor this into the function that wraps snap handlers
    -- TODO: this should happen at a lower level (simpleHttpServe?)
    chooseProxy conf = maybe handler0
                             (\ptype -> pickProxy ptype handler0)
                             (getProxyType conf)

    pickProxy NoProxy         = id
    pickProxy HaProxy         = id  -- we handle this case elsewhere
    pickProxy X_Forwarded_For = behindProxy Proxy.X_Forwarded_For


------------------------------------------------------------------------------
catch500 :: MonadSnap m => CmdlineConfig m a -> m () -> m ()
catch500 conf = flip E.catch $ fromJust $ getErrorHandler conf


------------------------------------------------------------------------------
compress :: MonadSnap m => CmdlineConfig m a -> m () -> m ()
compress conf = if fromJust $ getCompression conf then withCompression else id


------------------------------------------------------------------------------
-- | Starts serving HTTP using the given handler. The configuration is read
-- from the options given on the command-line, as returned by 'cmdlineConfig'.
-- This function never returns; to shut down the HTTP server, kill the
-- controlling thread.
quickHttpServe :: Snap () -> IO ()
quickHttpServe handler = do
    conf <- cmdlineConfig defaultCmdlineConfig
    httpServe conf handler


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
