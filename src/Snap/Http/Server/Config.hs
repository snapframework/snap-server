{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , completeConfig

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
  , getSSLBind
  , getSSLCert
  , getSSLKey
  , getSSLPort
  , getVerbose

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
  , setSSLBind
  , setSSLCert
  , setSSLKey
  , setSSLPort
  , setVerbose
  ) where


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
import           Prelude hiding (catch)
import           Snap.Core
import           Snap.Iteratee ((>==>), enumBuilder)
import           Snap.Internal.Debug (debug)
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
    show ConfigNoLog       = "ConfigNoLog"
    show (ConfigFileLog f) = "ConfigFileLog " ++ show f
    show (ConfigIoLog _)   = "ConfigIoLog"

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
        }

    a `mappend` b = Config
        { hostname       = ov hostname       a b
        , accessLog      = ov accessLog      a b
        , errorLog       = ov errorLog       a b
        , locale         = ov locale         a b
        , port           = ov port           a b
        , bind           = ov bind           a b
        , sslport        = ov sslport        a b
        , sslbind        = ov sslbind        a b
        , sslcert        = ov sslcert        a b
        , sslkey         = ov sslkey         a b
        , compression    = ov compression    a b
        , verbose        = ov verbose        a b
        , errorHandler   = ov errorHandler   a b
        , defaultTimeout = ov defaultTimeout a b
        , other          = ov other          a b
        , backend        = ov backend        a b
        }
      where
        ov f x y = getLast $! (mappend `on` (Last . f)) x y


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
-- | The hostname of the HTTP server
getHostname       :: Config m a -> Maybe ByteString
getHostname = hostname

-- | Path to the access log
getAccessLog      :: Config m a -> Maybe ConfigLog
getAccessLog = accessLog

-- | Path to the error log
getErrorLog       :: Config m a -> Maybe ConfigLog
getErrorLog = errorLog

-- | The locale to use
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


------------------------------------------------------------------------------
completeConfig :: (MonadSnap m) => Config m a -> IO (Config m a)
completeConfig config = do
    when noPort $ hPutStrLn stderr "no port specified, defaulting to port 8000"

    return $ cfg `mappend` cfg'

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
options :: MonadSnap m =>
           Config m a
        -> [OptDescr (Maybe (Config m a))]
options defaults =
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
             $ "use gzip compression on responses"
    , Option ['t'] ["timeout"]
             (ReqArg (\t -> Just $ mempty {
                              defaultTimeout = Just $ read t
                            }) "SECS")
             $ "set default timeout in seconds"
    , Option [] ["no-compression"]
             (NoArg $ Just $ setConfig setCompression False)
             $ "serve responses uncompressed"
    , Option ['v'] ["verbose"]
             (NoArg $ Just $ setConfig setVerbose True)
             $ "print server status updates to stderr"
    , Option ['q'] ["quiet"]
             (NoArg $ Just $ setConfig setVerbose False)
             $ "do not print anything to stderr"
    , Option ['h'] ["help"]
             (NoArg Nothing)
             $ "display this help and exit"
    ]
  where
    setConfig f c = f c mempty
    conf          = defaultConfig `mappend` defaults
    defaultC f    = maybe "" ((", default " ++) . show) $ f conf
    defaultO f    = maybe ", default off" ((", default " ++) . show) $ f conf


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
-- | Returns a 'Config' obtained from parsing the options specified on the
-- command-line.
--
-- On Unix systems, the locale is read from the @LANG@ environment variable.
commandLineConfig :: MonadSnap m =>
                     Config m a   -- ^ default configuration. This is combined
                                  -- with 'defaultConfig' to obtain default
                                  -- values to use if the given parameter is not
                                  -- specified on the command line. Usually it is
                                  -- fine to use 'emptyConfig' here.
                  -> IO (Config m a)
commandLineConfig defaults = do
    args <- getArgs
    prog <- getProgName

    let opts = options defaults

    result <- either (usage prog opts)
                     return
                     (case getOpt Permute opts args of
                        (f, _, []  ) -> maybe (Left []) Right $
                                        fmap mconcat $ sequence f
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
    usage prog opts errs = do
        let hdr = "Usage:\n  " ++ prog ++ " [OPTION...]\n\nOptions:"
        let msg = concat errs ++ usageInfo hdr opts
        hPutStrLn stderr msg
        exitFailure
#ifndef PORTABLE
    upToUtf8 = takeWhile $ \c -> isAlpha c || '_' == c
#endif
