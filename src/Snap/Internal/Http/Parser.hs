{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Http.Parser
  ( IRequest(..)
  , parseRequest
  , readChunkedTransferEncoding
  , iterParser
  , parseCookie
  , parseUrlEncoded
  , strictize
  ) where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad (liftM)
import           Control.Monad.Trans
import           Data.Attoparsec hiding (many, Result(..))
import           Data.Attoparsec.Enumerator
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Nums.Careless.Hex as Cvt
import           Data.Char
import           Data.List (foldl')
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Prelude hiding (head, take, takeWhile)
----------------------------------------------------------------------------
import           Snap.Internal.Http.Types
import           Snap.Internal.Debug
import           Snap.Internal.Iteratee.Debug
import           Snap.Internal.Parsing
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
parseRequest :: (Monad m) => Iteratee ByteString m (Maybe IRequest)
parseRequest = iterParser pRequest


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
    debug $ "chunkParserToEnumeratee: getting chunk"
    mbB <- getChunk
    debug $ "chunkParserToEnumeratee: getChunk was " ++ show mbB
    mbX <- peek
    debug $ "chunkParserToEnumeratee: .. and peek is " ++ show mbX


    maybe finishIt sendBS mbB

  where
    whatWasReturn (Continue _) = "continue"
    whatWasReturn (Yield _ z)  = "yield, with remainder " ++ show z
    whatWasReturn (Error e)    = "error, with " ++ show e

    sendBS s = do
        step' <- lift $ runIteratee $ enumBS s client
        debug $ "chunkParserToEnumeratee: after sending "
                  ++ show s ++ ", return was "
                  ++ whatWasReturn step'
        mbX <- peek
        debug $ "chunkParserToEnumeratee: .. and peek is " ++ show mbX
        chunkParserToEnumeratee getChunk step'

    finishIt = lift $ runIteratee $ enumEOF client


------------------------------------------------------------------------------
-- parse functions
------------------------------------------------------------------------------

-- theft alert: many of these routines adapted from Johan Tibell's hyena
-- package


------------------------------------------------------------------------------
-- | Parser for the internal request data type.
pRequest :: Parser (Maybe IRequest)
pRequest = (Just <$> pRequest') <|>
           (option "" crlf *> endOfInput *> pure Nothing)


------------------------------------------------------------------------------
pRequest' :: Parser IRequest
pRequest' = IRequest
               <$> (option "" crlf *> pMethod)  <* sp
               <*> pUri                         <* sp
               <*> pVersion                     <* crlf
               <*> pHeaders                     <* crlf

  -- note: the optional crlf is at the beginning because some older browsers
  -- send an extra crlf after a POST body


------------------------------------------------------------------------------
-- | Parser for the request method.
pMethod :: Parser Method
pMethod =     (OPTIONS <$ string "OPTIONS")
          <|> (GET     <$ string "GET")
          <|> (HEAD    <$ string "HEAD")
          <|> word8 (c2w 'P') *> ((POST <$ string "OST") <|>
                                  (PUT  <$ string "UT"))
          <|> (DELETE  <$ string "DELETE")
          <|> (TRACE   <$ string "TRACE")
          <|> (CONNECT <$ string "CONNECT")


------------------------------------------------------------------------------
-- | Parser for the request URI.
pUri :: Parser ByteString
pUri = takeWhile (not . isSpace . w2c)


------------------------------------------------------------------------------
-- | Parser for the request's HTTP protocol version.
pVersion :: Parser (Int, Int)
pVersion = string "HTTP/" *>
           liftA2 (,) (digit' <* word8 (c2w '.')) digit'
    where
      digit' = fmap digitToInt digit


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
    breakApart = (second (S.drop 1)) . S.break (== (c2w '='))

    parts :: [(ByteString,ByteString)]
    parts = map breakApart $ S.split (c2w '&') s

    urldecode = parseToCompletion pUrlEscaped

    decodeOne (a,b) = do
        a' <- urldecode a
        b' <- urldecode b
        return (a',b')

    decoded = catMaybes $ map decodeOne parts


