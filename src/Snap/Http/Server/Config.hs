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
    } deriving (Show)


data Flag = Flag
    { flagLocalHost   :: Maybe String
    , flagBindAddress :: Maybe String
    , flagPort        :: Maybe Int
    , flagUsage       :: Bool
    }

instance Monoid Flag where
    mempty = Flag Nothing Nothing Nothing False

    (Flag a1 b1 c1 d1) `mappend` (Flag a2 b2 c2 d2) =
        Flag (getLast $ Last a1 `mappend` Last a2)
             (getLast $ Last b1 `mappend` Last b2)
             (getLast $ Last c1 `mappend` Last c2)
             (d1 || d2)

flagLH :: String -> Flag
flagLH s = mempty { flagLocalHost = Just s }

flagBA :: String -> Flag
flagBA s = mempty { flagBindAddress = Just s }

flagPt :: String -> Flag
flagPt p = mempty { flagPort = Just (read p) }

flagHelp :: Flag
flagHelp = mempty { flagUsage = True }

fromStr :: String -> ByteString
fromStr = B.pack . map c2w

flags2config :: Flag -> Config
flags2config (Flag a b c _) =
    Config (maybe "localhost" fromStr a)
           (maybe "*" fromStr b)
           (fromMaybe 8888 c)


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
