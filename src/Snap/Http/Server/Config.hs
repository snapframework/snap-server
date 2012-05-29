{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

This module exports the 'Config' datatype, which you can use to configure the
Snap HTTP server.

-}

module Snap.Http.Server.Config
  ( Config
  , ConfigBackend(..)
  , ConfigLog(..)

  , emptyConfig
  , defaultConfig
  , commandLineConfig
  , extendedCommandLineConfig
  , completeConfig

  , optDescrs
  , fmapOpt

  , getAccessLog
  , getBackend
  , getBind
  , getCompression
  , getDefaultTimeout
  , getErrorHandler
  , getErrorLog
  , getHostname
  , getLocale
  , getOther
  , getPort
  , getProxyType
  , getSSLBind
  , getSSLCert
  , getSSLKey
  , getSSLPort
  , getVerbose
  , getStartupHook

  , setAccessLog
  , setBackend
  , setBind
  , setCompression
  , setDefaultTimeout
  , setErrorHandler
  , setErrorLog
  , setHostname
  , setLocale
  , setOther
  , setPort
  , setProxyType
  , setSSLBind
  , setSSLCert
  , setSSLKey
  , setSSLPort
  , setVerbose
  , setStartupHook

  , StartupHookData
  , emptyStartupHookData
  , getStartupHookSockets
  , getStartupHookConfig
  , setStartupHookSockets
  , setStartupHookConfig
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import           Control.Exception (SomeException)
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Network(Socket)
import           Prelude hiding (catch)
import           Snap.Core
import           Snap.Iteratee ((>==>), enumBuilder)
import           Snap.Internal.Debug (debug)
import           Snap.Util.Proxy
import           System.Console.GetOpt
import           System.Environment hiding (getEnv)
#ifndef PORTABLE
import           System.Posix.Env
#endif
import           System.Exit
import           System.IO
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server (requestErrorMessage)


------------------------------------------------------------------------------
-- | This datatype allows you to override which backend (either simple or
-- libev) to use. Most users will not want to set this, preferring to rely on
-- the compile-type default.
--
-- Note that if you specify the libev backend and have not compiled in support
-- for it, your server will fail at runtime.
data ConfigBackend = ConfigSimpleBackend
                   | ConfigLibEvBackend
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Data type representing the configuration of a logging target
data ConfigLog = ConfigNoLog                        -- ^ no logging
               | ConfigFileLog FilePath             -- ^ log to text file
               | ConfigIoLog (ByteString -> IO ())  -- ^ log custom IO handler

instance Show ConfigLog where
    show ConfigNoLog       = "no log"
    show (ConfigFileLog f) = "log to file " ++ show f
    show (ConfigIoLog _)   = "custom logging handler"

------------------------------------------------------------------------------
-- | A record type which represents partial configurations (for 'httpServe')
-- by wrapping all of its fields in a 'Maybe'. Values of this type are usually
-- constructed via its 'Monoid' instance by doing something like:
--
-- > setPort 1234 mempty
--
-- Any fields which are unspecified in the 'Config' passed to 'httpServe' (and
-- this is the norm) are filled in with default values from 'defaultConfig'.
data Config m a = Config
    { hostname       :: Maybe ByteString
    , accessLog      :: Maybe ConfigLog
    , errorLog       :: Maybe ConfigLog
    , locale         :: Maybe String
    , port           :: Maybe Int
    , bind           :: Maybe ByteString
    , sslport        :: Maybe Int
    , sslbind        :: Maybe ByteString
    , sslcert        :: Maybe FilePath
    , sslkey         :: Maybe FilePath
    , compression    :: Maybe Bool
    , verbose        :: Maybe Bool
    , errorHandler   :: Maybe (SomeException -> m ())
    , defaultTimeout :: Maybe Int
    , other          :: Maybe a
    , backend        :: Maybe ConfigBackend
    , proxyType      :: Maybe ProxyType
    , startupHook    :: Maybe (StartupHookData m a -> IO ())
    }

instance Show (Config m a) where
    show c = unlines [ "Config:"
                     , "hostname: "       ++ _hostname
                     , "accessLog: "      ++ _accessLog
                     , "errorLog: "       ++ _errorLog
                     , "locale: "         ++ _locale
                     , "port: "           ++ _port
                     , "bind: "           ++ _bind
                     , "sslport: "        ++ _sslport
                     , "sslbind: "        ++ _sslbind
                     , "sslcert: "        ++ _sslcert
                     , "sslkey: "         ++ _sslkey
                     , "compression: "    ++ _compression
                     , "verbose: "        ++ _verbose
                     , "defaultTimeout: " ++ _defaultTimeout
                     , "backend: "        ++ _backend
                     , "proxyType: "      ++ _proxyType
                     ]

      where
        _hostname       = show $ hostname       c
        _accessLog      = show $ accessLog      c
        _errorLog       = show $ errorLog       c
        _locale         = show $ locale         c
        _port           = show $ port           c
        _bind           = show $ bind           c
        _sslport        = show $ sslport        c
        _sslbind        = show $ sslbind        c
        _sslcert        = show $ sslcert        c
        _sslkey         = show $ sslkey         c
        _compression    = show $ compression    c
        _verbose        = show $ verbose        c
        _defaultTimeout = show $ defaultTimeout c
        _backend        = show $ backend        c
        _proxyType      = show $ proxyType      c


------------------------------------------------------------------------------
-- | Returns a completely empty 'Config'. Equivalent to 'mempty' from
-- 'Config''s 'Monoid' instance.
emptyConfig :: Config m a
emptyConfig = mempty


------------------------------------------------------------------------------
instance Monoid (Config m a) where
    mempty = Config
        { hostname       = Nothing
        , accessLog      = Nothing
        , errorLog       = Nothing
        , locale         = Nothing
        , port           = Nothing
        , bind           = Nothing
        , sslport        = Nothing
        , sslbind        = Nothing
        , sslcert        = Nothing
        , sslkey         = Nothing
        , compression    = Nothing
        , verbose        = Nothing
        , errorHandler   = Nothing
        , defaultTimeout = Nothing
        , other          = Nothing
        , backend        = Nothing
        , proxyType      = Nothing
        , startupHook    = Nothing
        }

    a `mappend` b = Config
        { hostname       = ov hostname
        , accessLog      = ov accessLog
        , errorLog       = ov errorLog
        , locale         = ov locale
        , port           = ov port
        , bind           = ov bind
        , sslport        = ov sslport
        , sslbind        = ov sslbind
        , sslcert        = ov sslcert
        , sslkey         = ov sslkey
        , compression    = ov compression
        , verbose        = ov verbose
        , errorHandler   = ov errorHandler
        , defaultTimeout = ov defaultTimeout
        , other          = ov other
        , backend        = ov backend
        , proxyType      = ov proxyType
        , startupHook    = ov startupHook
        }
      where
        ov f = getLast $! (mappend `on` (Last . f)) a b


------------------------------------------------------------------------------
-- | The 'Typeable1' instance is here so 'Config' values can be
-- dynamically loaded with Hint.
configTyCon :: TyCon
configTyCon = mkTyCon "Snap.Http.Server.Config.Config"
{-# NOINLINE configTyCon #-}

instance (Typeable1 m) => Typeable1 (Config m) where
    typeOf1 _ = mkTyConApp configTyCon [typeOf1 (undefined :: m ())]


------------------------------------------------------------------------------
-- | These are the default values for the options
defaultConfig :: MonadSnap m => Config m a
defaultConfig = mempty
    { hostname       = Just "localhost"
    , accessLog      = Just $ ConfigFileLog "log/access.log"
    , errorLog       = Just $ ConfigFileLog "log/error.log"
    , locale         = Just "en_US"
    , compression    = Just True
    , verbose        = Just True
    , errorHandler   = Just defaultErrorHandler
    , bind           = Just "0.0.0.0"
    , sslbind        = Just "0.0.0.0"
    , sslcert        = Just "cert.pem"
    , sslkey         = Just "key.pem"
    , defaultTimeout = Just 60
    }


------------------------------------------------------------------------------
-- | The hostname of the HTTP server. This field has the same format as an HTTP
-- @Host@ header; if a @Host@ header came in with the request, we use that,
-- otherwise we default to this value specified in the configuration.
getHostname       :: Config m a -> Maybe ByteString
getHostname = hostname

-- | Path to the access log
getAccessLog      :: Config m a -> Maybe ConfigLog
getAccessLog = accessLog

-- | Path to the error log
getErrorLog       :: Config m a -> Maybe ConfigLog
getErrorLog = errorLog

-- | Gets the locale to use. Locales are used on Unix only, to set the
-- @LANG@\/@LC_ALL@\/etc. environment variable. For instance if you set the
-- locale to \"@en_US@\", we'll set the relevant environment variables to
-- \"@en_US.UTF-8@\".
getLocale         :: Config m a -> Maybe String
getLocale = locale

-- | Returns the port to listen on (for http)
getPort           :: Config m a -> Maybe Int
getPort = port

-- | Returns the address to bind to (for http)
getBind           :: Config m a -> Maybe ByteString
getBind = bind

-- | Returns the port to listen on (for https)
getSSLPort        :: Config m a -> Maybe Int
getSSLPort = sslport

-- | Returns the address to bind to (for https)
getSSLBind        :: Config m a -> Maybe ByteString
getSSLBind = sslbind

-- | Path to the SSL certificate file
getSSLCert        :: Config m a -> Maybe FilePath
getSSLCert = sslcert

-- | Path to the SSL key file
getSSLKey         :: Config m a -> Maybe FilePath
getSSLKey = sslkey

-- | If set and set to True, compression is turned on when applicable
getCompression    :: Config m a -> Maybe Bool
getCompression = compression

-- | Whether to write server status updates to stderr
getVerbose        :: Config m a -> Maybe Bool
getVerbose = verbose

-- | A MonadSnap action to handle 500 errors
getErrorHandler   :: Config m a -> Maybe (SomeException -> m ())
getErrorHandler = errorHandler

getDefaultTimeout :: Config m a -> Maybe Int
getDefaultTimeout = defaultTimeout

getOther :: Config m a -> Maybe a
getOther = other

getBackend :: Config m a -> Maybe ConfigBackend
getBackend = backend

getProxyType :: Config m a -> Maybe ProxyType
getProxyType = proxyType

-- | An action that is run after the server has been started, given a
--   'StartupHookData'.
getStartupHook :: Config m a -> Maybe (StartupHookData m a -> IO ())
getStartupHook = startupHook


------------------------------------------------------------------------------
setHostname       :: ByteString              -> Config m a -> Config m a
setHostname x c = c { hostname = Just x }

setAccessLog      :: ConfigLog               -> Config m a -> Config m a
setAccessLog x c = c { accessLog = Just x }

setErrorLog       :: ConfigLog               -> Config m a -> Config m a
setErrorLog x c = c { errorLog = Just x }

setLocale         :: String                  -> Config m a -> Config m a
setLocale x c = c { locale = Just x }

setPort           :: Int                     -> Config m a -> Config m a
setPort x c = c { port = Just x }

setBind           :: ByteString              -> Config m a -> Config m a
setBind x c = c { bind = Just x }

setSSLPort        :: Int                     -> Config m a -> Config m a
setSSLPort x c = c { sslport = Just x }

setSSLBind        :: ByteString              -> Config m a -> Config m a
setSSLBind x c = c { sslbind = Just x }

setSSLCert        :: FilePath                -> Config m a -> Config m a
setSSLCert x c = c { sslcert = Just x }

setSSLKey         :: FilePath                -> Config m a -> Config m a
setSSLKey x c = c { sslkey = Just x }

setCompression    :: Bool                    -> Config m a -> Config m a
setCompression x c = c { compression = Just x }

setVerbose        :: Bool                    -> Config m a -> Config m a
setVerbose x c = c { verbose = Just x }

setErrorHandler   :: (SomeException -> m ()) -> Config m a -> Config m a
setErrorHandler x c = c { errorHandler = Just x }

setDefaultTimeout :: Int                     -> Config m a -> Config m a
setDefaultTimeout x c = c { defaultTimeout = Just x }

setOther          :: a                       -> Config m a -> Config m a
setOther x c = c { other = Just x }

setBackend        :: ConfigBackend           -> Config m a -> Config m a
setBackend x c = c { backend = Just x }

setProxyType      :: ProxyType               -> Config m a -> Config m a
setProxyType x c = c { proxyType = Just x }

setStartupHook    :: (StartupHookData m a -> IO ()) -> Config m a -> Config m a
setStartupHook x c = c { startupHook = Just x }


------------------------------------------------------------------------------

-- | Arguments passed to 'setStartupHook'.
data StartupHookData m a = StartupHookData
    { startupHookConfig :: Config m a
    , startupHookSockets :: [Socket]
    }

emptyStartupHookData :: StartupHookData m a
emptyStartupHookData = StartupHookData emptyConfig []

-- | The the 'Socket's opened by the server. There will be two 'Socket's for SSL connections, and one otherwise.
getStartupHookSockets :: StartupHookData m a -> [Socket]
getStartupHookSockets = startupHookSockets

-- The 'Config', after any command line parsing has been performed.
getStartupHookConfig :: StartupHookData m a -> Config m a
getStartupHookConfig = startupHookConfig

setStartupHookSockets :: [Socket] -> StartupHookData m a -> StartupHookData m a
setStartupHookSockets x c = c { startupHookSockets = x }

setStartupHookConfig :: Config m a -> StartupHookData m a -> StartupHookData m a
setStartupHookConfig x c = c { startupHookConfig = x }


------------------------------------------------------------------------------
completeConfig :: (MonadSnap m) => Config m a -> IO (Config m a)
completeConfig config = do
    when noPort $ hPutStrLn stderr
        "no port specified, defaulting to port 8000"

    return $! cfg `mappend` cfg'

  where
    cfg = defaultConfig `mappend` config

    sslVals = map ($ cfg) [ isJust . getSSLPort
                          , isJust . getSSLBind
                          , isJust . getSSLKey
                          , isJust . getSSLCert ]

    sslValid   = and sslVals
    noPort = isNothing (getPort cfg) && not sslValid

    cfg' = emptyConfig { port = if noPort then Just 8000 else Nothing }


------------------------------------------------------------------------------
bsFromString :: String -> ByteString
bsFromString = T.encodeUtf8 . T.pack


------------------------------------------------------------------------------
toString :: ByteString -> String
toString = T.unpack . T.decodeUtf8


------------------------------------------------------------------------------
-- | Returns a description of the snap command line options suitable for use
-- with "System.Console.GetOpt".
optDescrs :: MonadSnap m =>
             Config m a         -- ^ the configuration defaults.
          -> [OptDescr (Maybe (Config m a))]
optDescrs defaults =
    [ Option [] ["hostname"]
             (ReqArg (Just . setConfig setHostname . bsFromString) "NAME")
             $ "local hostname" ++ defaultC getHostname
    , Option ['b'] ["address"]
             (ReqArg (\s -> Just $ mempty { bind = Just $ bsFromString s })
                     "ADDRESS")
             $ "address to bind to" ++ defaultO bind
    , Option ['p'] ["port"]
             (ReqArg (\s -> Just $ mempty { port = Just $ read s}) "PORT")
             $ "port to listen on" ++ defaultO port
    , Option [] ["ssl-address"]
             (ReqArg (\s -> Just $ mempty { sslbind = Just $ bsFromString s })
                     "ADDRESS")
             $ "ssl address to bind to" ++ defaultO sslbind
    , Option [] ["ssl-port"]
             (ReqArg (\s -> Just $ mempty { sslport = Just $ read s}) "PORT")
             $ "ssl port to listen on" ++ defaultO sslport
    , Option [] ["ssl-cert"]
             (ReqArg (\s -> Just $ mempty { sslcert = Just s}) "PATH")
             $ "path to ssl certificate in PEM format" ++ defaultO sslcert
    , Option [] ["ssl-key"]
             (ReqArg (\s -> Just $ mempty { sslkey = Just s}) "PATH")
             $ "path to ssl private key in PEM format" ++ defaultO sslkey
    , Option [] ["access-log"]
             (ReqArg (Just . setConfig setAccessLog . ConfigFileLog) "PATH")
             $ "access log" ++ (defaultC $ getAccessLog)
    , Option [] ["error-log"]
             (ReqArg (Just . setConfig setErrorLog . ConfigFileLog) "PATH")
             $ "error log" ++ (defaultC $ getErrorLog)
    , Option [] ["no-access-log"]
             (NoArg $ Just $ setConfig setAccessLog ConfigNoLog)
             $ "don't have an access log"
    , Option [] ["no-error-log"]
             (NoArg $ Just $ setConfig setErrorLog ConfigNoLog)
             $ "don't have an error log"
    , Option ['c'] ["compression"]
             (NoArg $ Just $ setConfig setCompression True)
             $ "use gzip compression on responses" ++
               defaultB getCompression "compressed" "uncompressed"
    , Option ['t'] ["timeout"]
             (ReqArg (\t -> Just $ mempty {
                              defaultTimeout = Just $ read t
                            }) "SECS")
             $ "set default timeout in seconds" ++ defaultC defaultTimeout
    , Option [] ["no-compression"]
             (NoArg $ Just $ setConfig setCompression False)
             $ "serve responses uncompressed" ++
               defaultB compression "compressed" "uncompressed"
    , Option ['v'] ["verbose"]
             (NoArg $ Just $ setConfig setVerbose True)
             $ "print server status updates to stderr" ++
               defaultC getVerbose
    , Option ['q'] ["quiet"]
             (NoArg $ Just $ setConfig setVerbose False)
             $ "do not print anything to stderr" ++
               defaultB getVerbose "verbose" "quiet"
    , Option [] ["proxy"]
             (ReqArg (\t -> Just $ setConfig setProxyType $ read t)
                     "X_Forwarded_For")
             $ concat [ "Set --proxy=X_Forwarded_For if your snap application "
                      , "is behind an HTTP reverse proxy to ensure that "
                      , "rqRemoteAddr is set properly."
                      , defaultC getProxyType ]
    , Option ['h'] ["help"]
             (NoArg Nothing)
             $ "display this help and exit"
    ]

  where
    setConfig f c  = f c mempty
    conf           = defaultConfig `mappend` defaults
    defaultB f y n = maybe "" (\b -> ", default " ++ if b
                                                       then y
                                                       else n) $ f conf
    defaultC f     = maybe "" ((", default " ++) . show) $ f conf
    defaultO f     = maybe ", default off" ((", default " ++) . show) $ f conf


------------------------------------------------------------------------------
defaultErrorHandler :: MonadSnap m => SomeException -> m ()
defaultErrorHandler e = do
    debug "Snap.Http.Server.Config errorHandler:"
    req <- getRequest
    let sm = smsg req
    debug $ toString sm
    logError sm

    finishWith $ setContentType "text/plain; charset=utf-8"
               . setContentLength (fromIntegral $ B.length msg)
               . setResponseStatus 500 "Internal Server Error"
               . modifyResponseBody
                     (>==> enumBuilder (fromByteString msg))
               $ emptyResponse

  where
    smsg req = toByteString $ requestErrorMessage req e

    msg  = toByteString msgB
    msgB = mconcat [
             fromByteString "A web handler threw an exception. Details:\n"
           , fromShow e
           ]



------------------------------------------------------------------------------
-- | Returns a 'Config' obtained from parsing command-line options, using the
-- default Snap 'OptDescr' set.
--
-- On Unix systems, the locale is read from the @LANG@ environment variable.
commandLineConfig :: MonadSnap m
                  => Config m a
                      -- ^ default configuration. This is combined with
                      -- 'defaultConfig' to obtain default values to use if the
                      -- given parameter is specified on the command line.
                      -- Usually it is fine to use 'emptyConfig' here.
                  -> IO (Config m a)
commandLineConfig defaults = extendedCommandLineConfig (optDescrs defaults) f defaults
  where
    -- Here getOpt can ever change the "other" field, because we only use the
    -- Snap OptDescr list. The combining function will never be invoked.
    f = undefined


------------------------------------------------------------------------------
-- | Returns a 'Config' obtained from parsing command-line options, using the
-- default Snap 'OptDescr' set as well as a list of user OptDescrs. User
-- OptDescrs use the \"other\" field (accessible using 'getOther' and
-- 'setOther') to store additional command-line option state. These are
-- combined using a user-defined combining function.
--
-- On Unix systems, the locale is read from the @LANG@ environment variable.

extendedCommandLineConfig :: MonadSnap m
                          => [OptDescr (Maybe (Config m a))]
                             -- ^ User options.
                          -> (a -> a -> a)
                             -- ^ State for multiple invoked user command-line
                             -- options will be combined using this function.
                          -> Config m a
                             -- ^ default configuration. This is combined with
                             -- Snap's 'defaultConfig' to obtain default values
                             -- to use if the given parameter is specified on
                             -- the command line. Usually it is fine to use
                             -- 'emptyConfig' here.
                          -> IO (Config m a)
extendedCommandLineConfig opts combiningFunction defaults = do
    args <- getArgs
    prog <- getProgName

    result <- either (usage prog)
                     return
                     (case getOpt Permute opts args of
                        (f, _, []  ) -> maybe (Left []) Right $
                                        fmap (foldl' combine mempty) $
                                        sequence f
                        (_, _, errs) -> Left errs)

#ifndef PORTABLE
    lang <- getEnv "LANG"
    completeConfig $ mconcat [defaults,
                              mempty {locale = fmap upToUtf8 lang},
                              result]
#else
    completeConfig $ mconcat [defaults, result]
#endif

  where
    usage prog errs = do
        let hdr = "Usage:\n  " ++ prog ++ " [OPTION...]\n\nOptions:"
        let msg = concat errs ++ usageInfo hdr opts
        hPutStrLn stderr msg
        exitFailure
#ifndef PORTABLE
    upToUtf8 = takeWhile $ \c -> isAlpha c || '_' == c
#endif

    combine !a !b = a `mappend` b `mappend` newOther
      where
        -- combined is only a Just if both a and b have other fields, and then
        -- we use the combining function. Config's mappend picks the last
        -- "Just" in the other list.
        combined = do
            x <- getOther a
            y <- getOther b
            return $! combiningFunction x y

        newOther = mempty { other = combined }

fmapArg :: (a -> b) -> ArgDescr a -> ArgDescr b
fmapArg f (NoArg a) = NoArg (f a)
fmapArg f (ReqArg g s) = ReqArg (f . g) s
fmapArg f (OptArg g s) = OptArg (f . g) s

fmapOpt :: (a -> b) -> OptDescr a -> OptDescr b
fmapOpt f (Option s l d e) = Option s l (fmapArg f d) e


