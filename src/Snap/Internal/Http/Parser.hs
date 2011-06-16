{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ViewPatterns       #-}

module Snap.Internal.Http.Parser
  ( IRequest(..)
  , HttpParseException
  , parseRequest
  , readChunkedTransferEncoding
  , iterParser
  , parseCookie
  , parseUrlEncoded
  , strictize
  ) where


------------------------------------------------------------------------------
import           Control.Arrow (second)
import           Control.Exception
import           Control.Monad (liftM)
import           Control.Monad.Trans
import           Data.Attoparsec hiding (many, Result(..))
import           Data.Attoparsec.Enumerator
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe as S
import           Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Nums.Careless.Hex as Cvt
import           Data.Char
import           Data.List (foldl')
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Typeable
import           Prelude hiding (head, take, takeWhile)
----------------------------------------------------------------------------
import           Snap.Internal.Http.Types
import           Snap.Internal.Debug
import           Snap.Internal.Iteratee.Debug
import           Snap.Internal.Parsing hiding (pHeaders)
import           Snap.Iteratee hiding (map, take)


------------------------------------------------------------------------------
-- | an internal version of the headers part of an HTTP request
data IRequest = IRequest
    { iMethod :: Method
    , iRequestUri :: ByteString
    , iHttpVersion :: (Int,Int)
    , iRequestHeaders :: [(ByteString, ByteString)]
    }


------------------------------------------------------------------------------
instance Show IRequest where
    show (IRequest m u v r) =
        concat [ show m
               , " "
               , show u
               , " "
               , show v
               , " "
               , show r ]


------------------------------------------------------------------------------
data HttpParseException = HttpParseException String deriving (Typeable, Show)
instance Exception HttpParseException

------------------------------------------------------------------------------
parseRequest :: (Monad m) => Iteratee ByteString m (Maybe IRequest)
parseRequest = do
    eof <- isEOF
    if eof
      then return Nothing
      else do
        line <- pLine
        if S.null line
          then parseRequest
          else do
            let (!mStr,!s)   = bSp line
            let (!uri,!vStr) = bSp s

            !method <- methodFromString mStr

            let ver@(!_,!_) = pVer vStr

            hdrs    <- pHeaders
            return $ Just $ IRequest method uri ver hdrs

  where
    pVer s = if S.isPrefixOf "HTTP/" s
               then let (a,b) = bDot $ S.drop 5 s
                    in (read $ S.unpack a, read $ S.unpack b)
               else (1,0)

    isSp  = (== ' ')
    bSp   = splitWith isSp
    isDot = (== '.')
    bDot  = splitWith isDot


------------------------------------------------------------------------------
pLine :: (Monad m) => Iteratee ByteString m ByteString
pLine = continue $ k S.empty
  where
    k _ EOF = throwError $
              HttpParseException "parse error: expected line ending in crlf"
    k !pre (Chunks xs) =
        if S.null b
          then continue $ k a
          else yield a (Chunks [S.drop 2 b])
      where
        (!a,!b) = S.breakSubstring "\r\n" s
        !s      = S.append pre s'
        !s'     = S.concat xs


------------------------------------------------------------------------------
splitWith :: (Char -> Bool) -> ByteString -> (ByteString,ByteString)
splitWith !f !s = let (!a,!b) = S.break f s
                      !b'     = S.dropWhile f b
                  in (a, b')


------------------------------------------------------------------------------
pHeaders :: Monad m => Iteratee ByteString m [(ByteString,ByteString)]
pHeaders = do
    f <- go id
    return $! f []
  where
    go !dlistSoFar = {-# SCC "pHeaders/go" #-} do
        line <- pLine
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
            mbS  <- peek
            maybe (return dlist)
                  (\s -> if S.null s
                           then head >> pCont dlist
                           else if isCont $ w2c $ S.unsafeHead s
                                  then procCont dlist
                                  else return dlist)
                  mbS

        procCont !dlist = do
            line <- pLine
            let !t = trim line
            pCont (dlist . (" ":) . (t:))


------------------------------------------------------------------------------
methodFromString :: (Monad m) => ByteString -> Iteratee ByteString m Method
methodFromString "GET"     = return GET
methodFromString "POST"    = return POST
methodFromString "HEAD"    = return HEAD
methodFromString "PUT"     = return PUT
methodFromString "DELETE"  = return DELETE
methodFromString "TRACE"   = return TRACE
methodFromString "OPTIONS" = return OPTIONS
methodFromString "CONNECT" = return CONNECT
methodFromString s         = 
    throwError $ HttpParseException $ "Bad method '" ++ S.unpack s ++ "'"


------------------------------------------------------------------------------
readChunkedTransferEncoding :: (MonadIO m) =>
                               Enumeratee ByteString ByteString m a
readChunkedTransferEncoding =
    chunkParserToEnumeratee $
    iterateeDebugWrapper "pGetTransferChunk" $
    iterParser pGetTransferChunk


------------------------------------------------------------------------------
chunkParserToEnumeratee :: (MonadIO m) =>
                           Iteratee ByteString m (Maybe ByteString)
                        -> Enumeratee ByteString ByteString m a
chunkParserToEnumeratee getChunk client = do
    mbB <- getChunk
    maybe finishIt sendBS mbB

  where
    sendBS s = do
        step <- lift $ runIteratee $ enumBS s client
        chunkParserToEnumeratee getChunk step

    finishIt = lift $ runIteratee $ enumEOF client


------------------------------------------------------------------------------
-- parse functions
------------------------------------------------------------------------------

------------------------------------------------------------------------------
pGetTransferChunk :: Parser (Maybe ByteString)
pGetTransferChunk = do
    !hex <- liftM fromHex $ (takeWhile (isHexDigit . w2c))
    takeTill ((== '\r') . w2c)
    crlf
    if hex <= 0
      then return Nothing
      else do
          x <- take hex
          crlf
          return $ Just x
  where
    fromHex :: ByteString -> Int
    fromHex s = Cvt.hex (L.fromChunks [s])


------------------------------------------------------------------------------
-- COOKIE PARSING
------------------------------------------------------------------------------

-- these definitions try to mirror RFC-2068 (the HTTP/1.1 spec) and RFC-2109
-- (cookie spec): please point out any errors!

------------------------------------------------------------------------------
pCookies :: Parser [Cookie]
pCookies = do
    -- grab kvps and turn to strict bytestrings
    kvps <- pAvPairs

    return $ map toCookie $ filter (not . S.isPrefixOf "$" . fst) kvps

  where
    toCookie (nm,val) = Cookie nm val Nothing Nothing Nothing


------------------------------------------------------------------------------
parseCookie :: ByteString -> Maybe [Cookie]
parseCookie = parseToCompletion pCookies


------------------------------------------------------------------------------
-- application/x-www-form-urlencoded
------------------------------------------------------------------------------

------------------------------------------------------------------------------
parseUrlEncoded :: ByteString -> Map ByteString [ByteString]
parseUrlEncoded s = foldl' (\m (k,v) -> Map.insertWith' (++) k [v] m)
                           Map.empty
                           decoded
  where
    breakApart = (second (S.drop 1)) . S.break (== '=')

    parts :: [(ByteString,ByteString)]
    parts = map breakApart $ S.split '&' s

    urldecode = parseToCompletion pUrlEscaped

    decodeOne (a,b) = do
        a' <- urldecode a
        b' <- urldecode b
        return (a',b')

    decoded = catMaybes $ map decodeOne parts


