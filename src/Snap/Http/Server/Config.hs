{-# LANGUAGE OverloadedStrings #-}

module Snap.Http.Server.Config
  ( Config(..)
  , Flags(..)
  , readConfigFromCmdLineArgs
  , readFlagsFromCmdLineArgs
  , flagsToConfig
  ) where

import qualified Data.ByteString.Char8 as S
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.Monoid
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

------------------------------------------------------------------------------
-- | Holds http server configuration.
data Config = Config
    { configLocalHostname :: !ByteString -- ^ The name of the server
    , configBindAddress   :: !ByteString -- ^ The local interface to
                                         -- bind to
    , configListenPort    :: !Int -- ^ The local port to bind to
    , configAccessLog     :: !(Maybe FilePath) -- ^ The path to the access log
    , configErrorLog      :: !(Maybe FilePath) -- ^ The path to the error log
    , configVerbose       :: !Bool -- ^ Whether to write server status
                                   -- updates to standard out
    } deriving (Show)


------------------------------------------------------------------------------
-- | A monoid instance for use in building 'Config' structures.
data Flags = Flags
    { flagLocalHost   :: Maybe String
    , flagBindAddress :: Maybe String
    , flagPort        :: Maybe Int
    , flagAccessLog   :: Maybe String
    , flagErrorLog    :: Maybe String
    , flagUsage       :: Bool
    , flagVerbose     :: Bool
    }


------------------------------------------------------------------------------
instance Monoid Flags where
    mempty = Flags Nothing Nothing Nothing Nothing Nothing False False

    (Flags a1 b1 c1 d1 e1 f1 g1) `mappend` (Flags a2 b2 c2 d2 e2 f2 g2) =
        Flags (getLast $ Last a1 `mappend` Last a2)
              (getLast $ Last b1 `mappend` Last b2)
              (getLast $ Last c1 `mappend` Last c2)
              (getLast $ Last d1 `mappend` Last d2)
              (getLast $ Last e1 `mappend` Last e2)
              (f1 || f2)
              (g1 || g2)


------------------------------------------------------------------------------
-- | Create a flag with the local host attribute set.
flagLH :: String -> Flags
flagLH s = mempty { flagLocalHost = Just s }


------------------------------------------------------------------------------
-- | Create a flag with the bind address attribute set.
flagBA :: String -> Flags
flagBA s = mempty { flagBindAddress = Just s }


------------------------------------------------------------------------------
-- | Create a flag with the port attribute set.
flagPt :: String -> Flags
flagPt p = mempty { flagPort = Just (read p) }


------------------------------------------------------------------------------
-- | Create a flag with the access log attribute set.
flagAL :: String -> Flags
flagAL s = mempty { flagAccessLog = Just s }


------------------------------------------------------------------------------
-- | Create a flag with the error log attribute set.
flagEL :: String -> Flags
flagEL s = mempty { flagErrorLog = Just s }


------------------------------------------------------------------------------
-- | Create a flag with the help attribute set.
flagHelp :: Flags
flagHelp = mempty { flagUsage = True }


------------------------------------------------------------------------------
-- | Create a flag with the verbose attribute set.
flagV :: Flags
flagV = mempty { flagVerbose = True }


------------------------------------------------------------------------------
-- | Convert 'Flags' to a 'Config'.
flagsToConfig :: Flags -> Config
flagsToConfig (Flags a b c d e _ g) =
    Config (maybe "localhost" S.pack a)
           (maybe "0.0.0.0" S.pack b)
           (fromMaybe 8000 c)
           d
           e
           g


------------------------------------------------------------------------------
options :: [OptDescr Flags]
options =
    [ Option "l" ["localHostname"]
                 (ReqArg flagLH "STR")
                 "local hostname, default 'localhost'"
    , Option "p" ["listenPort"]
                 (ReqArg flagPt "NUM")
                 "port to listen on, default 8000"
    , Option "b" ["bindAddress"]
                 (ReqArg flagBA "STR")
                 "address to bind to, default '0.0.0.0'"
    , Option "a" ["accessLog"]
                 (ReqArg flagAL "STR")
                 "access log in the 'combined' format, optional"
    , Option "e" ["errorLog"]
                 (ReqArg flagEL "STR")
                 "error log, optional"
    , Option "v" ["verbose"]
                 (NoArg flagV)
                 "print server status updates to standard out"
    , Option "h" ["help"]
                 (NoArg flagHelp)
                 "display this usage statement"
    ]


------------------------------------------------------------------------------
-- | Read the command line arguments and parse 'Flags' out of them.
readFlagsFromCmdLineArgs :: IO Flags
readFlagsFromCmdLineArgs = do
    argv     <- getArgs
    progName <- getProgName

    case getOpt Permute options argv of
        (f, _, []  ) -> withFlags progName f
        (_, _, errs) -> bombout progName errs
  where
    bombout progName errs = do
        let hdr = "\nUsage: " ++ progName ++ " [OPTIONS]"
        let msg = concat errs ++ usageInfo hdr options
        hPutStrLn stderr msg
        exitFailure

    withFlags progName fs = do
        let f = mconcat fs
        if flagUsage f
            then bombout progName []
            else return f


------------------------------------------------------------------------------
-- | Read the command line arguments and parse a 'Config' out of them.
readConfigFromCmdLineArgs :: IO Config
readConfigFromCmdLineArgs = fmap flagsToConfig readFlagsFromCmdLineArgs
