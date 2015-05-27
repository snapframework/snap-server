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
import           Control.Applicative               ((<$>), (<|>))
import           Control.Concurrent                (killThread, newEmptyMVar, newMVar, putMVar, readMVar, withMVar)
import           Control.Concurrent.Extended       (forkIOLabeledWithUnmaskBs)
import           Control.Exception                 (SomeException, bracket, catch, finally, mask, mask_)
import qualified Control.Exception.Lifted          as L
import           Control.Monad                     (liftM, when, (=<<))
import           Control.Monad.Trans               (MonadIO)
import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Char8             as S
import qualified Data.ByteString.Lazy.Char8        as L
import           Data.Maybe                        (catMaybes, fromJust, fromMaybe)
import           Data.Version                      (showVersion)
import           Data.Word                         (Word64)
import           Network.Socket                    (Socket, sClose)
import           Prelude                           (Bool (..), Eq (..), IO, Maybe (..), Monad (..), Show (..), String, const, flip, fst, id, mapM, mapM_, maybe, snd, unzip3, zip, ($), ($!), (++), (.))
import           System.IO                         (hFlush, hPutStrLn, stderr)
#ifndef PORTABLE
import           System.Posix.Env
#endif
------------------------------------------------------------------------------
import           Data.ByteString.Builder           (Builder, toLazyByteString)
------------------------------------------------------------------------------
import qualified Paths_snap_server                 as V
import           Snap.Core                         (MonadSnap (..), Request, Response, Snap, rqClientAddr, rqHeaders, rqMethod, rqURI, rqVersion, rspStatus)
import           Snap.Http.Server.Config           (Config, ConfigLog (..), commandLineConfig, completeConfig, defaultConfig, getAccessLog, getAccessLogHandler, getBind, getCompression, getDefaultTimeout, getErrorHandler, getErrorLog, getErrorLogHandler, getHostname, getLocale, getOther, getPort, getProxyType, getSSLBind, getSSLCert, getSSLChainCert, getSSLKey, getSSLPort, getStartupHook, getVerbose)
import qualified Snap.Http.Server.Types            as Ty
import           Snap.Internal.Debug               (debug)
import           Snap.Internal.Http.Server.Config  (AccessLogHandler, ErrorLogHandler, ProxyType (..), emptyStartupInfo, setStartupConfig, setStartupSockets)
import           Snap.Internal.Http.Server.Session (httpAcceptLoop, snapToServerHandler)
import qualified Snap.Internal.Http.Server.Socket  as Sock
import qualified Snap.Internal.Http.Server.TLS     as TLS
import           Snap.Internal.Http.Server.Types   (AcceptFunc, ServerConfig, ServerHandler)
import qualified Snap.Types.Headers                as H
import           Snap.Util.GZip                    (withCompression)
import           Snap.Util.Proxy                   (behindProxy)
import qualified Snap.Util.Proxy                   as Proxy
import           System.FastLogger                 (combinedLogEntry, logMsg, newLoggerWithCustomErrorFunction, stopLogger, timestampedLogEntry)


------------------------------------------------------------------------------
-- | A short string describing the Snap server version
snapServerVersion :: ByteString
snapServerVersion = S.pack $! showVersion V.version


------------------------------------------------------------------------------
rawHttpServe :: ServerHandler s  -- ^ server handler
             -> ServerConfig s   -- ^ server config
             -> [AcceptFunc]     -- ^ listening server backends
             -> IO ()
rawHttpServe h cfg loops = do
    mvars <- mapM (const newEmptyMVar) loops
    mask $ \restore -> bracket (mapM runLoop $ mvars `zip` loops)
                               (\mvTids -> do
                                   mapM_ (killThread . snd) mvTids
                                   mapM_ (readMVar . fst) mvTids)
                               (const $ restore $ mapM_ readMVar mvars)
  where
    -- parents and children have a mutual suicide pact
    runLoop (mvar, loop) = do
        tid <- forkIOLabeledWithUnmaskBs
               "snap-server http master thread" $
               \r -> (r $ httpAcceptLoop h cfg loop) `finally` putMVar mvar ()
        return (mvar, tid)

------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler. This function never
-- returns; to shut down the HTTP server, kill the controlling thread.
--
-- This function is like 'httpServe' except it doesn't setup compression,
-- reverse proxy address translation (via 'Snap.Util.Proxy.behindProxy'), or
-- the error handler; this allows it to be used from 'MonadSnap'.
simpleHttpServe :: MonadSnap m => Config m a -> Snap () -> IO ()
simpleHttpServe config handler = do
    conf <- completeConfig config
    let output = when (fromJust $ getVerbose conf) . hPutStrLn stderr
    (descrs, sockets, afuncs) <- unzip3 <$> listeners conf
    mapM_ (output . ("Listening on " ++) . S.unpack) descrs

    go conf sockets afuncs `finally` (mask_ $ do
        output "\nShutting down.."
        mapM_ (eatException . sClose) sockets)

  where
    eatException :: IO a -> IO ()
    eatException act =
        let r0 = return $! ()
        in (act >> r0) `catch` \(_::SomeException) -> r0

    --------------------------------------------------------------------------
    -- FIXME: this logging code *sucks*
    --------------------------------------------------------------------------
    debugE :: (MonadIO m) => ByteString -> m ()
    debugE s = debug $ "Error: " ++ S.unpack s


    --------------------------------------------------------------------------
    logE :: ErrorLogHandler -> Maybe (ByteString -> IO ()) -> Builder -> IO ()
    logE elh elog b = let x = S.concat $ L.toChunks $ toLazyByteString b
                      in (maybe debugE (\l s -> debugE s >> logE' elh l s) elog) x

    --------------------------------------------------------------------------
    logE' :: ErrorLogHandler -> (ByteString -> IO ()) -> ByteString -> IO ()
    logE' elh logger s = logger =<< elh s

    --------------------------------------------------------------------------
    logA :: AccessLogHandler
         -> Maybe (ByteString -> IO ())
         -> Request
         -> Response
         -> Word64
         -> IO ()
    logA alh alog = maybe (\_ _ _ -> return $! ()) (logA' alh) alog

    --------------------------------------------------------------------------
    logA' alh logger req rsp cl = logger =<< alh req rsp cl

    --------------------------------------------------------------------------
    accessHandler conf = fromMaybe defaultAccessLogHandler (getAccessLogHandler conf)
    errorHandler conf = fromMaybe defaultErrorLogHandler (getErrorLogHandler conf)

    --------------------------------------------------------------------------
    go conf sockets afuncs = do
        let tout = fromMaybe 60 $ getDefaultTimeout conf
        let shandler = snapToServerHandler handler
        let ah = accessHandler conf
        let eh = errorHandler conf

        setUnicodeLocale $ fromJust $ getLocale conf

        withLoggers (fromJust $ getAccessLog conf)
                    (fromJust $ getErrorLog conf) $ \(alog, elog) -> do
            let scfg = Ty.setDefaultTimeout tout .
                       Ty.setLocalHostname (fromJust $ getHostname conf) .
                       Ty.setLogAccess (logA ah alog) .
                       Ty.setLogError (logE eh elog) $
                       Ty.emptyServerConfig
            maybe (return $! ())
                  ($ mkStartupInfo sockets conf)
                  (getStartupHook conf)
            rawHttpServe shandler scfg afuncs

    --------------------------------------------------------------------------
    mkStartupInfo sockets conf =
        setStartupSockets sockets $
        setStartupConfig conf emptyStartupInfo

    --------------------------------------------------------------------------
    maybeSpawnLogger f (ConfigFileLog fp) =
        liftM Just $ newLoggerWithCustomErrorFunction f fp
    maybeSpawnLogger _ _                  = return Nothing

    --------------------------------------------------------------------------
    maybeIoLog (ConfigIoLog a) = Just a
    maybeIoLog _               = Nothing

    --------------------------------------------------------------------------
    withLoggers afp efp act =
        bracket (do mvar <- newMVar ()
                    let f s = withMVar mvar
                                (const $ S.hPutStr stderr s >> hFlush stderr)
                    alog <- maybeSpawnLogger f afp
                    elog <- maybeSpawnLogger f efp
                    return (alog, elog))
                (\(alog, elog) -> do
                    maybe (return ()) stopLogger alog
                    maybe (return ()) stopLogger elog)
                (\(alog, elog) -> act ( liftM logMsg alog <|> maybeIoLog afp
                                      , liftM logMsg elog <|> maybeIoLog efp))
{-# INLINE simpleHttpServe #-}

------------------------------------------------------------------------------
defaultAccessLogHandler :: AccessLogHandler
defaultAccessLogHandler req rsp cl = do
   let hdrs      = rqHeaders req
   let host      = rqClientAddr req
   let user      = Nothing -- TODO we don't do authentication yet
   let (v, v')   = rqVersion req
   let ver       = S.concat [ "HTTP/", bshow v, ".", bshow v' ]
   let method    = bshow (rqMethod req)
   let reql      = S.intercalate " " [ method, rqURI req, ver ]
   let status    = rspStatus rsp
   let referer   = H.lookup "referer" hdrs
   let userAgent = fromMaybe "-" $ H.lookup "user-agent" hdrs

   combinedLogEntry host user reql status cl referer userAgent

------------------------------------------------------------------------------
defaultErrorLogHandler :: ErrorLogHandler
defaultErrorLogHandler = timestampedLogEntry

------------------------------------------------------------------------------
listeners :: Config m a -> IO [(ByteString, Socket, AcceptFunc)]
listeners conf = TLS.withTLS $ do
  let fs = catMaybes [httpListener, httpsListener]
  mapM (\(str, mkAfunc) -> do (sock, afunc) <- mkAfunc
                              return $! (str, sock, afunc)) fs
  where
    httpsListener = do
        b    <- getSSLBind conf
        p    <- getSSLPort conf
        cert <- getSSLCert conf
        chainCert <- getSSLChainCert conf
        key  <- getSSLKey conf
        return (S.concat [ "https://"
                         , b
                         , ":"
                         , bshow p ],
                do (sock, ctx) <- TLS.bindHttps b p cert chainCert key
                   return (sock, TLS.httpsAcceptFunc sock ctx)
                )
    httpListener = do
        p <- getPort conf
        b <- getBind conf
        return (S.concat [ "http://"
                         , b
                         , ":"
                         , bshow p ],
                do sock <- Sock.bindSocket b p
                   if getProxyType conf == Just HaProxy
                     then return (sock, Sock.haProxyAcceptFunc sock)
                     else return (sock, Sock.httpAcceptFunc sock))


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler, with settings from
-- the 'Config' passed in. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
httpServe :: Config Snap a -> Snap () -> IO ()
httpServe config handler0 = do
    conf <- completeConfig config
    let !handler = chooseProxy conf
    let serve    = compress conf . catch500 conf $ handler
    simpleHttpServe conf serve

  where
    chooseProxy conf = maybe handler0
                             (\ptype -> pickProxy ptype handler0)
                             (getProxyType conf)

    pickProxy NoProxy         = id
    pickProxy HaProxy         = id  -- we handle this case elsewhere
    pickProxy X_Forwarded_For = behindProxy Proxy.X_Forwarded_For


------------------------------------------------------------------------------
catch500 :: MonadSnap m => Config m a -> m () -> m ()
catch500 conf = flip L.catch $ fromJust $ getErrorHandler conf


------------------------------------------------------------------------------
compress :: MonadSnap m => Config m a -> m () -> m ()
compress conf = if fromJust $ getCompression conf then withCompression else id


------------------------------------------------------------------------------
-- | Starts serving HTTP using the given handler. The configuration is read
-- from the options given on the command-line, as returned by
-- 'commandLineConfig'. This function never returns; to shut down the HTTP
-- server, kill the controlling thread.
quickHttpServe :: Snap () -> IO ()
quickHttpServe handler = do
    conf <- commandLineConfig defaultConfig
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

------------------------------------------------------------------------------
bshow :: (Show a) => a -> ByteString
bshow = S.pack . show
