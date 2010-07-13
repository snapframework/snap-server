{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

This module exports the 'Config' datatype which represents partially-specified
configurations of \"serve\" functions which run 'Snap' actions in 'IO'.

-}

module Snap.Http.Server.Config
  ( Config
  , emptyConfig
  , defaultConfig
  , completeConfig
  , commandLineConfig

  , getHostname
  , getAddress
  , getPort
  , getAccessLog
  , getErrorLog
  , getLocale
  , getCompression
  , getVerbose
  , getErrorHandler
  , getOther

  , setHostname
  , setAddress
  , setPort
  , setAccessLog
  , setErrorLog
  , setLocale
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
import           Data.Maybe
import           Data.Monoid
import           Prelude hiding (catch)
import           Snap.Types
import           Snap.Iteratee ((>.), enumBS)
import           System.Console.GetOpt
import           System.Environment hiding (getEnv)
#ifndef PORTABLE
import           System.Posix.Env
#endif
import           System.Exit
import           System.IO


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
    , address      :: Maybe ByteString
      -- ^ The local interface to bind to
    , port         :: Maybe Int
      -- ^ The local port to bind to
    , accessLog    :: Maybe (Maybe FilePath)
      -- ^ The path to the access log
    , errorLog     :: Maybe (Maybe FilePath)
      -- ^ The path to the error log
    , locale       :: Maybe String
      -- ^ The locale to use
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
        , showM "address" . address
        , showM "port" . port
        , showM "accessLog" . accessLog
        , showM "errorLog" . errorLog
        , showM "locale" . locale
        , showM "compression" . compression
        , showM "verbose" . verbose
        , showM "errorHandler" . fmap (const ()) . errorHandler
        ]) ++ "}"
      where
        showM s = maybe "" ((++) (s ++ " = ") . show)


------------------------------------------------------------------------------
-- | Returns a completely empty 'Config'. Equivalent to 'mempty' from
-- 'Config''s 'Monoid' instance.
emptyConfig :: MonadSnap m => Config m a
emptyConfig = mempty


------------------------------------------------------------------------------
instance MonadSnap m => Monoid (Config m a) where
    mempty = Config
        { hostname     = Nothing
        , address      = Nothing
        , port         = Nothing
        , accessLog    = Nothing
        , errorLog     = Nothing
        , locale       = Nothing
        , compression  = Nothing
        , verbose      = Nothing
        , errorHandler = Nothing
        , other        = Nothing
        }

    a `mappend` b = Config
        { hostname     = (hostname     b) `mplus` (hostname     a)
        , address      = (address      b) `mplus` (address      a)
        , port         = (port         b) `mplus` (port         a)
        , accessLog    = (accessLog    b) `mplus` (accessLog    a)
        , errorLog     = (errorLog     b) `mplus` (errorLog     a)
        , locale       = (locale       b) `mplus` (locale       a)
        , compression  = (compression  b) `mplus` (compression  a)
        , verbose      = (verbose      b) `mplus` (verbose      a)
        , errorHandler = (errorHandler b) `mplus` (errorHandler a)
        , other        = (other        b) `mplus` (other        a)
        }


------------------------------------------------------------------------------
-- | These are the default values for all the fields in 'Config'.
--
-- > hostname     = "localhost"
-- > address      = "0.0.0.0"
-- > port         = 8000
-- > accessLog    = "log/access.log"
-- > errorLog     = "log/error.log"
-- > locale       = "en_US"
-- > compression  = True
-- > verbose      = True
-- > errorHandler = prints the error message
--
defaultConfig :: MonadSnap m => Config m a
defaultConfig = Config
    { hostname     = Just "localhost"
    , address      = Just "0.0.0.0"
    , port         = Just 8000
    , accessLog    = Just $ Just "log/access.log"
    , errorLog     = Just $ Just "log/error.log"
    , locale       = Just "en_US"
    , compression  = Just True
    , verbose      = Just True
    , errorHandler = Just $ \e -> let msg = U.fromString $ show e in
        finishWith $ setContentType "text/plain; charset=utf-8"
        . setContentLength (fromIntegral $ B.length msg)
        . setResponseStatus 500 "Internal Server Error"
        . modifyResponseBody (>. (enumBS $ mappend
            "A web handler threw an exception. Details:\n" msg))
        $ emptyResponse
    , other        = Nothing
    }


------------------------------------------------------------------------------
-- | Completes a partial 'Config' by filling in the unspecified values with
-- the default values from 'defaultConfig'.
completeConfig :: MonadSnap m => Config m a -> Config m a
completeConfig = mappend defaultConfig


------------------------------------------------------------------------------
-- | A description of the command-line options accepted by
-- 'commandLineConfig'.
--
-- The 'Config' parameter is just for specifying any default values which are
-- to override those in 'defaultConfig'. This is so the usage message can
-- accurately inform the user what the default values for the options are. In
-- most cases, you will probably just end up passing 'mempty' for this
-- parameter.
--
-- The return type is a list of options describing a @'Maybe' ('Config' m)@
-- as opposed to a @'Config' m@, because if the @--help@ option is given,
-- the set of command-line options no longer describe a config, but an action
-- (printing out the usage message).
options :: MonadSnap m => Config m a -> [OptDescr (Maybe (Config m a))]
options defaults =
    [ Option [] ["hostname"]
             (ReqArg (Just . flip setHostname mempty . U.fromString) "NAME")
             $ "local hostname" ++ default_ getHostname
    , Option ['b'] ["address"]
             (ReqArg (Just . flip setAddress mempty . U.fromString) "ADDRESS")
             $ "address to bind to" ++ default_ getAddress
    , Option ['p'] ["port"]
             (ReqArg (Just . flip setPort mempty . read) "PORT")
             $ "port to listen on" ++ default_ getPort
    , Option [] ["access-log"]
             (ReqArg (Just . flip setAccessLog mempty . Just) "PATH")
             $ "access log" ++ (default_ $ join . getAccessLog)
    , Option [] ["error-log"]
             (ReqArg (Just . flip setErrorLog mempty . Just) "PATH")
             $ "error log" ++ (default_ $ join . getErrorLog)
    , Option [] ["no-access-log"]
             (NoArg $ Just $ setErrorLog Nothing mempty)
             $ "don't have an access log"
    , Option [] ["no-error-log"]
             (NoArg $ Just $ setAccessLog Nothing mempty)
             $ "don't have an error log"
    , Option ['c'] ["compression"]
             (NoArg $ Just $ setCompression True mempty)
             $ "use gzip compression on responses"
    , Option [] ["no-compression"]
             (NoArg $ Just $ setCompression False mempty)
             $ "serve responses uncompressed"
    , Option ['v'] ["verbose"]
             (NoArg $ Just $ setVerbose True mempty)
             $ "print server status updates to stderr"
    , Option ['q'] ["quiet"]
             (NoArg $ Just $ setVerbose False mempty)
             $ "do not print anything to stderr"
    , Option ['h'] ["help"]
             (NoArg Nothing)
             $ "display this help and exit"
    ]
  where
    conf       = completeConfig defaults
    default_ f = maybe "" ((", default " ++) . show) $ f conf


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

#ifndef PORTABLE
    lang <- getEnv "LANG"
    return $ mconcat [defaults, result, mempty {locale = fmap upToUtf8 lang}]
#else
    return $ mconcat [defaults, result]
#endif

  where
    opts = options defaults
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
getAddress :: MonadSnap m => Config m a -> Maybe ByteString
getAddress = address


------------------------------------------------------------------------------
getPort :: MonadSnap m => Config m a -> Maybe Int
getPort = port


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
setAddress :: MonadSnap m => ByteString -> Config m a -> Config m a
setAddress a m = m {address = Just a}


------------------------------------------------------------------------------
setPort :: MonadSnap m => Int -> Config m a -> Config m a
setPort a m = m {port = Just a}


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
