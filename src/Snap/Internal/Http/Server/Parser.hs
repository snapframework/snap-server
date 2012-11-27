{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE Rank2Types         #-}

module Snap.Internal.Http.Server.Parser
  ( IRequest(..)
  , HttpParseException(..)
  , parseRequest
  , readChunkedTransferEncoding
  , parseFromStream
  , parseCookie
  , parseUrlEncoded
  , strictize
  ) where

------------------------------------------------------------------------------
import           Control.Exception                (Exception, throwIO)
import qualified Control.Exception                as E
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString.Char8 (Parser, decimal, hexadecimal,
                                                   parseOnly, take, takeTill,
                                                   takeWhile)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as S
import           Data.ByteString.Internal         (w2c)
import qualified Data.ByteString.Unsafe           as S
import           Data.Typeable                    (Typeable)
import           Prelude                          hiding (head, take, takeWhile)
import           System.IO.Streams                (InputStream)
import qualified System.IO.Streams                as Streams
import           System.IO.Streams.Attoparsec     (parseFromStream)
----------------------------------------------------------------------------
import           Snap.Internal.Http.Types         (Method (..))
import           Snap.Internal.Parsing            (crlf, parseCookie,
                                                   parseUrlEncoded, strictize,
                                                   trim)


------------------------------------------------------------------------------
-- | an internal version of the headers part of an HTTP request
data IRequest = IRequest
    { iMethod         :: !Method
    , iRequestUri     :: {-# UNPACK #-} !ByteString
    , iHttpMajor      :: {-# UNPACK #-} !Int
    , iHttpMinor      :: {-# UNPACK #-} !Int
    , iHost           :: !(Maybe ByteString)
    , iRequestHeaders :: ![(ByteString, ByteString)]
    }
  deriving (Eq)

------------------------------------------------------------------------------
instance Show IRequest where
    show (IRequest m u major minor host r) =
        concat [ show m
               , " "
               , show u
               , " "
               , show major
               , "."
               , show minor
               , " "
               , show host
               , " "
               , show r ]


------------------------------------------------------------------------------
data HttpParseException = HttpParseException String deriving (Typeable, Show)
instance Exception HttpParseException


------------------------------------------------------------------------------
parseRequest :: InputStream ByteString -> IO (Maybe IRequest)
parseRequest input = do
    eof <- Streams.atEOF input
    if eof
      then return Nothing
      else do
        line <- pLine input
        if S.null line
          then parseRequest input
          else do
            let (!mStr,!s)      = bSp line
            let (!uri, !vStr)   = bSp s
            let method          = methodFromString mStr
            (!major, !minor)   <- pVer vStr
            let (host, uri')    = getHost uri

            hdrs    <- pHeaders input
            return $! Just $! IRequest method uri' major minor host hdrs

  where
    getHost s | S.isPrefixOf "http://" s
                  = let s'            = S.drop 7 s
                        (!host, !uri) = S.break (== '/') s'
                    in (Just host, uri)
              | otherwise = (Nothing, s)


    pVer s = if S.isPrefixOf "HTTP/" s
               then either (throwIO . HttpParseException)
                           return
                           (parseOnly pVers (S.drop 5 s))
               else return $! (1, 0)

    isSp  = (== ' ')
    bSp   = splitWith isSp

    pVers = do
        !x <- decimal
        _  <- takeWhile (== '.')
        !y <- decimal
        return (x, y)


------------------------------------------------------------------------------
pLine :: InputStream ByteString -> IO ByteString
pLine input = go id
  where
    noCRLF = HttpParseException "parse error: expected line ending in crlf"

    eolchar c = c == '\r'

    go !dl = do
        !mb <- Streams.read input
        !s  <- maybe (throwIO noCRLF) return mb

        let (!a,!b) = S.break eolchar s

        if S.null b
          then go (dl . (a:))
          else do
            Streams.unRead (S.drop 2 b) input
            return $! S.concat $! dl [a]


------------------------------------------------------------------------------
splitWith :: (Char -> Bool) -> ByteString -> (ByteString,ByteString)
splitWith !f !s = let (!a,!b) = S.break f s
                      !b'     = S.dropWhile f b
                  in (a, b')


------------------------------------------------------------------------------
pHeaders :: InputStream ByteString -> IO [(ByteString,ByteString)]
pHeaders input = do
    f <- go id
    return $! f []

  where
    go !dlistSoFar = {-# SCC "pHeaders/go" #-} do
        line <- pLine input
        if S.null line
          then return dlistSoFar
          else do
            let (!k,!v) = pOne line
            vf <- pCont id
            let vs = vf []
            let !v' = S.concat (v:vs)
            go (dlistSoFar . ((k,v'):))

      where
        pOne s = let (k,v) = splitWith (== ':') s
                 in (trim k, trim v)

        isCont c = c == ' ' || c == '\t'

        pCont !dlist = do
            mbS  <- Streams.peek input
            maybe (return dlist)
                  (\s -> if S.null s
                           then Streams.read input >> pCont dlist
                           else if isCont $ w2c $ S.unsafeHead s
                                  then procCont dlist
                                  else return dlist)
                  mbS

        procCont !dlist = do
            line <- pLine input
            let !t = trim line
            pCont (dlist . (" ":) . (t:))


------------------------------------------------------------------------------
methodFromString :: ByteString -> Method
methodFromString "GET"     = GET
methodFromString "POST"    = POST
methodFromString "HEAD"    = HEAD
methodFromString "PUT"     = PUT
methodFromString "DELETE"  = DELETE
methodFromString "TRACE"   = TRACE
methodFromString "OPTIONS" = OPTIONS
methodFromString "CONNECT" = CONNECT
methodFromString "PATCH"   = PATCH
methodFromString s         = Method s


------------------------------------------------------------------------------
readChunkedTransferEncoding :: InputStream ByteString
                            -> IO (InputStream ByteString)
readChunkedTransferEncoding input =
    Streams.makeInputStream $ parseFromStream pGetTransferChunk input


                             ---------------------
                             -- parse functions --
                             ---------------------

------------------------------------------------------------------------------
-- We treat chunks larger than this from clients as a denial-of-service attack.
-- 256kB should be enough buffer.
mAX_CHUNK_SIZE :: Int
mAX_CHUNK_SIZE = (2::Int)^(18::Int)


------------------------------------------------------------------------------
pGetTransferChunk :: Parser (Maybe ByteString)
pGetTransferChunk = do
    !hex <- hexadecimal
    !_   <- takeTill (== '\r')
    void crlf
    if hex >= mAX_CHUNK_SIZE
      then return $! E.throw $! HttpParseException $!
           "pGetTransferChunk: chunk of size " ++ show hex ++ " too long."
      else if hex <= 0
        then return Nothing
        else do
            -- now safe to take this many bytes.
            !x <- take hex
            void crlf
            return $! Just x
