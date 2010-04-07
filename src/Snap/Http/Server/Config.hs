{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snap.Http.Server.Config
  ( Config(..)
  , readConfigFromCmdLineArgs
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Internal (c2w)
import           Data.ByteString.Char8 ()
import           Data.Maybe
import           Data.Monoid
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

data Config = Config
    { localHostname :: !ByteString
    , bindAddress   :: !ByteString
    , listenPort    :: !Int
    , accessLog     :: !(Maybe FilePath)
    , errorLog      :: !(Maybe FilePath)
    } deriving (Show)


data Flag = Flag
    { flagLocalHost   :: Maybe String
    , flagBindAddress :: Maybe String
    , flagPort        :: Maybe Int
    , flagAccessLog   :: Maybe String
    , flagErrorLog    :: Maybe String
    , flagUsage       :: Bool
    }

instance Monoid Flag where
    mempty = Flag Nothing Nothing Nothing Nothing Nothing False

    (Flag a1 b1 c1 d1 e1 f1) `mappend` (Flag a2 b2 c2 d2 e2 f2) =
        Flag (getLast $ Last a1 `mappend` Last a2)
             (getLast $ Last b1 `mappend` Last b2)
             (getLast $ Last c1 `mappend` Last c2)
             (getLast $ Last d1 `mappend` Last d2)
             (getLast $ Last e1 `mappend` Last e2)
             (f1 || f2)

flagLH :: String -> Flag
flagLH s = mempty { flagLocalHost = Just s }

flagBA :: String -> Flag
flagBA s = mempty { flagBindAddress = Just s }

flagPt :: String -> Flag
flagPt p = mempty { flagPort = Just (read p) }

flagAL :: String -> Flag
flagAL s = mempty { flagAccessLog = Just s }

flagEL :: String -> Flag
flagEL s = mempty { flagErrorLog = Just s }

flagHelp :: Flag
flagHelp = mempty { flagUsage = True }

fromStr :: String -> ByteString
fromStr = B.pack . map c2w

flags2config :: Flag -> Config
flags2config (Flag a b c d e _) =
    Config (maybe "localhost" fromStr a)
           (maybe "*" fromStr b)
           (fromMaybe 8888 c)
           d
           e


options :: [OptDescr Flag]
options =
    [ Option "l" ["localHostname"]
                 (ReqArg flagLH "STR")
                 "local hostname, default 'localhost'"
    , Option "p" ["listenPort"]
                 (ReqArg flagPt "NUM")
                 "port to listen on, default 8888"
    , Option "b" ["bindAddress"]
                 (ReqArg flagBA "STR")
                 "address to bind to, default '*'"
    , Option "a" ["accessLog"]
                 (ReqArg flagAL "STR")
                 "access log in the 'combined' format, optional"
    , Option "e" ["errorLog"]
                 (ReqArg flagEL "STR")
                 "error log, optional"
    , Option "h" ["help"]
                 (NoArg flagHelp)
                 "display this usage statement" ]


readConfigFromCmdLineArgs :: String     -- ^ application description, e.g.
                                        --   \"Foo applet v0.2\"
                          -> IO Config
readConfigFromCmdLineArgs appName = do
    argv     <- getArgs
    progName <- getProgName

    case getOpt Permute options argv of
      (f,_,[]  ) -> withFlags progName f
      (_,_,errs) -> bombout progName errs
  where
    bombout progName errs = do
        let hdr = appName ++ "\n\nUsage: " ++ progName ++ " [OPTIONS]"
        let msg = concat errs ++ usageInfo hdr options
        hPutStrLn stderr msg
        exitFailure

    withFlags progName fs = do
        let f = mconcat fs
        if flagUsage f
           then bombout progName []
           else return $ flags2config f
