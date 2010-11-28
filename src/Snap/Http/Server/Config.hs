{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

This module exports the 'Config' datatype which represents partially-specified
configurations of \"serve\" functions which run 'Snap' actions in 'IO'.

-}

module Snap.Http.Server.Config
  ( Config
  , ConfigListen(..)
  , ConfigBackend(..)
  , emptyConfig
  , defaultConfig
  , completeConfig
  , commandLineConfig

  , getHostname
  , getListen
  , getAccessLog
  , getErrorLog
  , getLocale
  , getBackend
  , getCompression
  , getVerbose
  , getErrorHandler
  , getOther

  , setHostname
  , addListen
  , setAccessLog
  , setErrorLog
  , setLocale
  , setBackend
  , setCompression
  , setVerbose
  , setErrorHandler
  , setOther
  ) where

import           Control.Exception (SomeException)
import           Control.Monad
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.List
import           Data.Monoid
import           Prelude hiding (catch)
import           Snap.Types
import           Snap.Iteratee ((>==>), enumBS)
import           System.Console.GetOpt
import           System.Environment hiding (getEnv)
#ifndef PORTABLE
import           System.Posix.Env
#endif
import           System.Exit
import           System.IO

------------------------------------------------------------------------------
-- | A data type to store the bind address and port to listen on.
--
--  For SSL support, it also stores the path to the certificate in PEM format
--  and the path to the private key in PEM format
data ConfigListen = ListenHttp  ByteString Int
                  | ListenHttps ByteString Int FilePath FilePath
instance Show ConfigListen where
    show (ListenHttp b p) = "http(" ++ show b ++ ":" ++ show p ++ ")"
    show (ListenHttps b p c k) = "https(" ++ show b ++ ":" ++ show p ++
                                     ", cert = " ++ show c ++
                                     ", key = " ++ show k ++ ")"

------------------------------------------------------------------------------
-- | A data type to record which backend event loop should be used when
-- serving data.
data ConfigBackend = ConfigSimpleBackend
                   | ConfigLibEvBackend
    deriving (Eq,Show)

------------------------------------------------------------------------------
-- | A record type which represents partial configurations (for 'httpServe')
-- by wrapping all of its fields in a 'Maybe'. Values of this type are usually
-- constructed via its 'Monoid' instance by doing something like:
--
-- > setPort 9000 mempty
--
-- Any fields which are unspecified in the 'Config' passed to 'httpServe' (and
-- this is the norm) are filled in with default values from 'defaultConfig'.
data MonadSnap m => Config m a = Config
    { hostname     :: Maybe ByteString
      -- ^ The name of the server
    , listen       :: [ConfigListen]
      -- ^ The local interfaces to listen on
    , accessLog    :: Maybe (Maybe FilePath)
      -- ^ The path to the access log
    , errorLog     :: Maybe (Maybe FilePath)
      -- ^ The path to the error log
    , locale       :: Maybe String
      -- ^ The locale to use
    , backend      :: Maybe ConfigBackend
      -- ^ The backend to use
    , compression  :: Maybe Bool
      -- ^ Whether to use compression
    , verbose      :: Maybe Bool
      -- ^ Whether to write server status updates to stderr
    , errorHandler :: Maybe (SomeException -> m ())
      -- ^ A MonadSnap action to handle 500 errors
    , other        :: Maybe a
      -- ^ This is for any other state needed to initialize a custom server
    }


------------------------------------------------------------------------------
instance MonadSnap m => Show (Config m a) where
    show c = "Config {" ++ concat (intersperse ", " $ filter (/="") $ map ($c)
        [ showM "hostname" . hostname
        , showL "listen" . listen
        , showM "accessLog" . accessLog
        , showM "errorLog" . errorLog
        , showM "locale" . locale
        , showM "backend" . backend
        , showM "compression" . compression
        , showM "verbose" . verbose
        , showM "errorHandler" . fmap (const ()) . errorHandler
        ]) ++ "}"
      where
        showM s   = maybe "" ((++) (s ++ " = ") . show)
        showL s l = s ++ " = " ++ show l


------------------------------------------------------------------------------
-- | Returns a completely empty 'Config'. Equivalent to 'mempty' from
-- 'Config''s 'Monoid' instance.
emptyConfig :: MonadSnap m => Config m a
emptyConfig = mempty


------------------------------------------------------------------------------
instance MonadSnap m => Monoid (Config m a) where
    mempty = Config
        { hostname     = Nothing
        , listen       = []
        , accessLog    = Nothing
        , errorLog     = Nothing
        , locale       = Nothing
        , backend      = Nothing
        , compression  = Nothing
        , verbose      = Nothing
        , errorHandler = Nothing
        , other        = Nothing
        }

    a `mappend` b = Config
        { hostname     = (hostname     b) `mplus` (hostname     a)
        , listen       = (listen       b)   ++    (listen       a)
        , accessLog    = (accessLog    b) `mplus` (accessLog    a)
        , errorLog     = (errorLog     b) `mplus` (errorLog     a)
        , locale       = (locale       b) `mplus` (locale       a)
        , backend      = (backend      b) `mplus` (backend      a)
        , compression  = (compression  b) `mplus` (compression  a)
        , verbose      = (verbose      b) `mplus` (verbose      a)
        , errorHandler = (errorHandler b) `mplus` (errorHandler a)
        , other        = (other        b) `mplus` (other        a)
        }


------------------------------------------------------------------------------
-- | These are the default values for all the fields in 'Config'.
--
-- > hostname     = "localhost"
-- > listen       = []
-- > accessLog    = "log/access.log"
-- > errorLog     = "log/error.log"
-- > locale       = "en_US"
-- > backend      = Nothing (the backend is selected based on compile options)
-- > compression  = True
-- > verbose      = True
-- > errorHandler = prints the error message
--
defaultConfig :: MonadSnap m => Config m a
defaultConfig = Config
    { hostname     = Just "localhost"
    , listen       = []
    , accessLog    = Just $ Just "log/access.log"
    , errorLog     = Just $ Just "log/error.log"
    , locale       = Just "en_US"
    , backend      = Nothing
    , compression  = Just True
    , verbose      = Just True
    , errorHandler = Just $ \e -> do
        let err = U.fromString $ show e
            msg = mappend "A web handler threw an exception. Details:\n" err
        finishWith $ setContentType "text/plain; charset=utf-8"
                   . setContentLength (fromIntegral $ B.length msg)
                   . setResponseStatus 500 "Internal Server Error"
                   . modifyResponseBody (>==> enumBS msg)
                   $ emptyResponse
    , other        = Nothing
    }


------------------------------------------------------------------------------
-- | Completes a partial 'Config' by filling in the unspecified values with
-- the default values from 'defaultConfig'.  Also, if no listeners are
-- specified, adds a http listener on 0.0.0.0:8000
completeConfig :: MonadSnap m => Config m a -> Config m a
completeConfig c = case listen c' of
                    [] -> addListen (ListenHttp "0.0.0.0" 8000) c'
                    _  -> c'
    where c' = mappend defaultConfig c

------------------------------------------------------------------------------
-- | A data structure used during command-line option parsing
--
-- The Config data type allows a list of listen ports, but the command line
-- options only allow one http and one https listener.  This data structure
-- is used during option parsing
data MonadSnap m => OptionData m a = OptionData
    { config  :: Config m a
    , bind    :: Maybe ByteString
    , port    :: Maybe Int
    , sslbind :: Maybe ByteString
    , sslport :: Maybe Int
    , sslcert :: Maybe FilePath
    , sslkey  :: Maybe FilePath
    }


------------------------------------------------------------------------------
instance MonadSnap m => Monoid (OptionData m a) where
    mempty = OptionData
        { config  = mempty
        , bind    = Nothing
        , port    = Nothing
        , sslbind = Nothing
        , sslport = Nothing
        , sslcert = Nothing
        , sslkey  = Nothing
        }

    a `mappend` b = OptionData
        { config       = (config       b) `mappend` (config       a)
        , bind         = (bind         b) `mplus`   (bind         a)
        , port         = (port         b) `mplus`   (port         a)
        , sslbind      = (sslbind      b) `mplus`   (sslbind      a)
        , sslport      = (sslport      b) `mplus`   (sslport      a)
        , sslcert      = (sslcert      b) `mplus`   (sslcert      a)
        , sslkey       = (sslkey       b) `mplus`   (sslkey       a)
        }

------------------------------------------------------------------------------
-- | These are the default values for the options
defaultOptions :: MonadSnap m => OptionData m a
defaultOptions = OptionData
    { config  = defaultConfig
    , bind    = Just "0.0.0.0"
    , port    = Just 8000
    , sslbind = Just "0.0.0.0"
    , sslport = Nothing
    , sslcert = Just "cert.pem"
    , sslkey  = Just "key.pem"
    }


------------------------------------------------------------------------------
-- | Convert options to config
optionsToConfig :: MonadSnap m => OptionData m a -> Config m a
optionsToConfig o = mconcat $ [config o] ++ http ++ https
    where lhttp  = maybe2 [] ListenHttp  (bind o) (port o)
          lhttps = maybe4 [] ListenHttps (sslbind o)
                                         (sslport o)
                                         (sslcert o)
                                         (sslkey  o)
          http  = map (flip addListen mempty) lhttp
          https = map (flip addListen mempty) lhttps

          maybe2 _ f (Just a) (Just b) = [f a b]
          maybe2 d _ _        _        = d
          maybe4 _ f (Just a) (Just b) (Just c) (Just d) = [f a b c d]
          maybe4 d _ _        _        _        _        = d

------------------------------------------------------------------------------
-- | Convert config to options
configToOptions :: MonadSnap m => Config m a -> OptionData m a
configToOptions c = OptionData
    { config  = c
    , bind    = Nothing
    , port    = Nothing
    , sslbind = Nothing
    , sslport = Nothing
    , sslcert = Nothing
    , sslkey  = Nothing
    }

------------------------------------------------------------------------------
-- | A description of the command-line options accepted by
-- 'commandLineConfig'.
--
-- The 'OptionData' parameter is just for specifying any default values which are
-- to override those in 'defaultOptions'. This is so the usage message can
-- accurately inform the user what the default values for the options are. In
-- most cases, you will probably just end up passing 'mempty' for this
-- parameter.
--
-- The return type is a list of options describing a @'Maybe' ('OptionData' m)@
-- as opposed to a @'OptionData' m@, because if the @--help@ option is given,
-- the set of command-line options no longer describe a config, but an action
-- (printing out the usage message).
options :: MonadSnap m => OptionData m a -> [OptDescr (Maybe (OptionData m a))]
options defaults =
    [ Option [] ["hostname"]
             (ReqArg (Just . setConfig setHostname . U.fromString) "NAME")
             $ "local hostname" ++ defaultC getHostname
    , Option ['b'] ["address"]
             (ReqArg (\s -> Just $ mempty { bind = Just $ U.fromString s }) "ADDRESS")
             $ "address to bind to" ++ defaultO bind
    , Option ['p'] ["port"]
             (ReqArg (\s -> Just $ mempty { port = Just $ read s}) "PORT")
             $ "port to listen on" ++ defaultO port
    , Option [] ["ssl-address"]
             (ReqArg (\s -> Just $ mempty { sslbind = Just $ U.fromString s }) "ADDRESS")
             $ "ssl address to bind to" ++ defaultO sslbind
    , Option [] ["ssl-port"]
             (ReqArg (\s -> Just $ mempty { sslport = Just $ read s}) "PORT")
             $ "ssl port to listen on" ++ defaultO sslport
    , Option [] ["ssl-cert"]
             (ReqArg (\s -> Just $ mempty { sslcert = Just $ read s}) "PATH")
             $ "path to ssl certificate in PEM format" ++ defaultO sslcert
    , Option [] ["ssl-key"]
             (ReqArg (\s -> Just $ mempty { sslkey = Just $ read s}) "PATH")
             $ "path to ssl private key in PEM format" ++ defaultO sslkey
    , Option [] ["access-log"]
             (ReqArg (Just . setConfig setAccessLog . Just) "PATH")
             $ "access log" ++ (defaultC $ join . getAccessLog)
    , Option [] ["error-log"]
             (ReqArg (Just . setConfig setErrorLog . Just) "PATH")
             $ "error log" ++ (defaultC $ join . getErrorLog)
    , Option [] ["no-access-log"]
             (NoArg $ Just $ setConfig setErrorLog Nothing)
             $ "don't have an access log"
    , Option [] ["no-error-log"]
             (NoArg $ Just $ setConfig setAccessLog Nothing)
             $ "don't have an error log"
    , Option ['c'] ["compression"]
             (NoArg $ Just $ setConfig setCompression True)
             $ "use gzip compression on responses"
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
    setConfig f c = configToOptions $ f c mempty
    conf          = completeConfig $ config defaults
    opts          = mappend defaultOptions defaults
    defaultC f    = maybe "" ((", default " ++) . show) $ f conf
    defaultO f    = maybe ", default off" ((", default " ++) . show) $ f opts


------------------------------------------------------------------------------
-- | This returns a 'Config' gotten from parsing the options specified on the
-- command-line.
--
-- The 'Config' parameter is just for specifying any default values which are
-- to override those in 'defaultConfig'. This is so the usage message can
-- accurately inform the user what the default values for the options are. In
-- most cases, you will probably just end up passing 'mempty' for this
-- parameter.
--
-- On Unix systems, the locale is read from the @LANG@ environment variable.
commandLineConfig :: MonadSnap m => Config m a -> IO (Config m a)
commandLineConfig defaults = do
    args <- getArgs
    prog <- getProgName

    result <- either (usage prog) return $ case getOpt Permute opts args of
        (f, _, []  ) -> maybe (Left []) Right $ fmap mconcat $ sequence f
        (_, _, errs) -> Left errs

    let result' = optionsToConfig $ mappend defaultOptions result

#ifndef PORTABLE
    lang <- getEnv "LANG"
    return $ mconcat [defaults, result', mempty {locale = fmap upToUtf8 lang}]
#else
    return $ mconcat [defaults, result']
#endif

  where
    opts = options $ configToOptions defaults
    usage prog errs = do
        let hdr = "Usage:\n  " ++ prog ++ " [OPTION...]\n\nOptions:"
        let msg = concat errs ++ usageInfo hdr opts
        hPutStrLn stderr msg
        exitFailure
    upToUtf8 = takeWhile $ \c -> isAlpha c || '_' == c


------------------------------------------------------------------------------
getHostname :: MonadSnap m => Config m a -> Maybe ByteString
getHostname = hostname


------------------------------------------------------------------------------
getListen :: MonadSnap m => Config m a -> [ConfigListen]
getListen = listen


------------------------------------------------------------------------------
getAccessLog :: MonadSnap m => Config m a -> Maybe (Maybe FilePath)
getAccessLog = accessLog


------------------------------------------------------------------------------
getErrorLog :: MonadSnap m => Config m a -> Maybe (Maybe FilePath)
getErrorLog = errorLog


------------------------------------------------------------------------------
getLocale :: MonadSnap m => Config m a -> Maybe String
getLocale = locale


------------------------------------------------------------------------------
getBackend :: MonadSnap m => Config m a -> Maybe ConfigBackend
getBackend = backend

------------------------------------------------------------------------------
getCompression :: MonadSnap m => Config m a -> Maybe Bool
getCompression = compression


------------------------------------------------------------------------------
getVerbose :: MonadSnap m => Config m a -> Maybe Bool
getVerbose = verbose


------------------------------------------------------------------------------
getErrorHandler :: MonadSnap m => Config m a -> Maybe (SomeException -> m ())
getErrorHandler = errorHandler


------------------------------------------------------------------------------
getOther :: MonadSnap m => Config m a -> Maybe a
getOther = other


------------------------------------------------------------------------------
setHostname :: MonadSnap m => ByteString -> Config m a -> Config m a
setHostname a m = m {hostname = Just a}


------------------------------------------------------------------------------
addListen :: MonadSnap m => ConfigListen -> Config m a -> Config m a
addListen a m = m {listen = a : listen m}


------------------------------------------------------------------------------
setAccessLog :: MonadSnap m => Maybe FilePath -> Config m a -> Config m a
setAccessLog a m = m {accessLog = Just a}


------------------------------------------------------------------------------
setErrorLog :: MonadSnap m => Maybe FilePath -> Config m a -> Config m a
setErrorLog a m = m {errorLog = Just a}


------------------------------------------------------------------------------
setLocale :: MonadSnap m => String -> Config m a -> Config m a
setLocale a m = m {locale = Just a}


------------------------------------------------------------------------------
setBackend :: MonadSnap m => ConfigBackend -> Config m a -> Config m a
setBackend a m = m { backend = Just a}


------------------------------------------------------------------------------
setCompression :: MonadSnap m => Bool -> Config m a -> Config m a
setCompression a m = m {compression = Just a}


------------------------------------------------------------------------------
setVerbose :: MonadSnap m => Bool -> Config m a -> Config m a
setVerbose a m = m {verbose = Just a}


------------------------------------------------------------------------------
setErrorHandler :: MonadSnap m => (SomeException -> m ()) -> Config m a
                -> Config m a
setErrorHandler a m = m {errorHandler = Just a}


------------------------------------------------------------------------------
setOther :: MonadSnap m => a -> Config m a -> Config m a
setOther a m = m {other = Just a}
