{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}

module Snap.Internal.Http.Parser
  ( IRequest(..)
  , parseRequest
  , readChunkedTransferEncoding
  , parserToIteratee
  , parseCookie
  , parseUrlEncoded
  , writeChunkedTransferEncoding
  , strictize
  ) where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad (liftM)
import "monads-fd" Control.Monad.Trans
import           Data.Attoparsec hiding (many, Result(..))
import           Data.Attoparsec.Iteratee
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Nums.Careless.Hex as Cvt
import           Data.Char
import           Data.List (foldl')
import           Data.Int
import           Data.Iteratee.WrappedByteString
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import qualified Data.Vector.Unboxed as Vec
import           Data.Vector.Unboxed (Vector)
import           Data.Word (Word8, Word64)
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Prelude hiding (take, takeWhile)
------------------------------------------------------------------------------
import           Snap.Internal.Http.Types hiding (Enumerator)
import           Snap.Iteratee hiding (take, foldl', filter)



------------------------------------------------------------------------------
-- | an internal version of the headers part of an HTTP request
data IRequest = IRequest
    { iMethod :: Method
    , iRequestUri :: ByteString
    , iHttpVersion :: (Int,Int)
    , iRequestHeaders :: [(ByteString, ByteString)]
    }

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
parseRequest :: (Monad m) => Iteratee m (Maybe IRequest)
parseRequest = parserToIteratee pRequest


readChunkedTransferEncoding :: (Monad m) => Enumerator m a
readChunkedTransferEncoding iter = do
      i <- chunkParserToEnumerator (parserToIteratee pGetTransferChunk)
                                   iter

      return i 


toHex :: Int64 -> ByteString
toHex !i' = S.reverse s
  where
    !i     = abs i'
    (!s,_) = S.unfoldrN 16 f (fromIntegral i)

    f :: Word64 -> Maybe (Word8, Word64)
    f d = if d == 0
            then Nothing
            else Just (ch, theRest)

      where
        low4    = fromIntegral $ d .&. 0xf
        ch      = if low4 >= 10
                    then c2w 'a' + low4 - 10
                    else c2w '0' + low4
        theRest = (d .&. (complement 0xf)) `shiftR` 4


-- | Given an iteratee, produces a new one that wraps chunks sent to it with a
-- chunked transfer-encoding. Example usage:
--
-- FIXME: sample output no longer looks like this, we buffer now
--
-- > > (writeChunkedTransferEncoding
-- >     (enumLBS (L.fromChunks ["foo","bar","quux"]))
-- >     stream2stream) >>=
-- >     run >>=
-- >     return . fromWrap
-- >
-- > Chunk "3\r\nfoo\r\n3\r\nbar\r\n4\r\nquux\r\n0\r\n\r\n" Empty
--
writeChunkedTransferEncoding :: ForeignPtr CChar
                             -> Enumerator IO a
                             -> Enumerator IO a
writeChunkedTransferEncoding _buf enum it = do
    i'    <- wrap it
    --(i,_) <- unsafeBufferIterateeWithBuffer buf i'
    (i,_) <- bufferIteratee i'
    enum i

  where
    wrap iter = return $ IterateeG $ \s ->
        case s of
          (EOF Nothing) -> do
              v <- runIter iter (Chunk $ toWrap "0\r\n\r\n")
              i <- checkIfDone return v
              runIter i (EOF Nothing)
          (EOF e) -> return $ Cont undefined e
          (Chunk (WrapBS x)) -> do
              let n = S.length x
              if n == 0
                then do
                    i' <- wrap iter
                    return $ Cont i' Nothing
                else do
                  let o = S.concat [ toHex (toEnum n)
                                   , "\r\n"
                                   , x
                                   , "\r\n" ]
                  v <- runIter iter (Chunk $ WrapBS o)
                  i <- checkIfDone wrap v
                  return $ Cont i Nothing


chunkParserToEnumerator :: (Monad m) =>
                           Iteratee m (Maybe ByteString)
                        -> Iteratee m a
                        -> m (Iteratee m a)
chunkParserToEnumerator getChunk client = return $ do
    mbB <- getChunk
    maybe (finishIt client) (sendBS client) mbB

  where
    sendBS iter s = do
        v <- lift $ runIter iter (Chunk $ toWrap $ L.fromChunks [s])

        case v of
          (Done _ (EOF (Just e))) -> throwErr e

          (Done x _) -> return x

          (Cont _ (Just e)) -> throwErr e

          (Cont k Nothing) -> joinIM $
                              chunkParserToEnumerator getChunk k

    finishIt iter = do
        e <- lift $ sendEof iter

        case e of
          Left x  -> throwErr x
          Right x -> return x

    sendEof iter = do
        v <- runIter iter (EOF Nothing)

        return $ case v of
          (Done _ (EOF (Just e))) -> Left e
          (Done x _)              -> Right x
          (Cont _ (Just e))       -> Left e
          (Cont _ _)              -> Left $ Err $ "divergent iteratee"


------------------------------------------------------------------------------
-- parse functions
------------------------------------------------------------------------------

-- theft alert: many of these routines adapted from Johan Tibell's hyena
-- package

-- | Parsers for different tokens in an HTTP request.
sp, digit, letter :: Parser Word8
sp       = word8 $ c2w ' '
digit    = satisfy (isDigit . w2c)
letter   = satisfy (isAlpha . w2c)

untilEOL :: Parser ByteString
untilEOL = takeWhile notend
  where
    notend d = let c = w2c d in not $ c == '\r' || c == '\n'

crlf :: Parser ByteString
crlf = string "\r\n"

-- | Parser for zero or more spaces.
spaces :: Parser [Word8]
spaces = many sp

pSpaces :: Parser ByteString
pSpaces = takeWhile (isSpace . w2c)

-- | Parser for the internal request data type.
pRequest :: Parser (Maybe IRequest)
pRequest = (Just <$> pRequest') <|> (endOfInput *> pure Nothing)

pRequest' :: Parser IRequest
pRequest' = IRequest
               <$> (option "" crlf *> pMethod)  <* sp
               <*> pUri                         <* sp
               <*> pVersion                     <* crlf
               <*> pHeaders                     <* crlf

  -- note: the optional crlf is at the beginning because some older browsers
  -- send an extra crlf after a POST body


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

-- | Parser for the request URI.
pUri :: Parser ByteString
pUri = takeWhile (not . isSpace . w2c)

-- | Parser for the request's HTTP protocol version.
pVersion :: Parser (Int, Int)
pVersion = string "HTTP/" *>
           liftA2 (,) (digit' <* word8 (c2w '.')) digit'
    where
      digit' = fmap (digitToInt . w2c) digit

fieldChars :: Parser ByteString
fieldChars = takeWhile isFieldChar
  where
    isFieldChar c = (Vec.!) fieldCharTable (fromEnum c)

fieldCharTable :: Vector Bool
fieldCharTable = Vec.generate 256 f
  where
    f d = let c=toEnum d in (isDigit c) || (isAlpha c) || c == '-' || c == '_'


-- | Parser for request headers.
pHeaders :: Parser [(ByteString, ByteString)]
pHeaders = many header
  where
    header = {-# SCC "pHeaders/header" #-}
             liftA2 (,)
                 fieldName
                 (word8 (c2w ':') *> spaces *> contents)

    fieldName = {-# SCC "pHeaders/fieldName" #-}
                liftA2 S.cons letter fieldChars

    contents = {-# SCC "pHeaders/contents" #-}
               liftA2 S.append
                   (untilEOL <* crlf)
                   (continuation <|> pure S.empty)

    isLeadingWS w = {-# SCC "pHeaders/isLeadingWS" #-}
                    elem w wstab

    wstab = map c2w " \t"

    leadingWhiteSpace = {-# SCC "pHeaders/leadingWhiteSpace" #-}
                        takeWhile1 isLeadingWS

    continuation = {-# SCC "pHeaders/continuation" #-}
                   liftA2 S.cons
                          (leadingWhiteSpace *> pure (c2w ' '))
                          contents


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

{-# INLINE matchAll #-}
matchAll :: [ Char -> Bool ] -> Char -> Bool
matchAll x c = and $ map ($ c) x

{-# INLINE isToken #-}
isToken :: Char -> Bool
isToken c = (Vec.!) tokenTable (fromEnum c)
  where
    tokenTable :: Vector Bool
    tokenTable = Vec.generate 256 (f . toEnum)

    f = matchAll [ isAscii
                 , not . isControl
                 , not . isSpace 
                 , not . flip elem [ '(', ')', '<', '>', '@', ',', ';'
                                   , ':', '\\', '\"', '/', '[', ']'
                                   , '?', '=', '{', '}' ]
                 ]

{-# INLINE isRFCText #-}
isRFCText :: Char -> Bool
isRFCText = not . isControl

pToken :: Parser ByteString
pToken = takeWhile (isToken . w2c)


pQuotedString :: Parser ByteString
pQuotedString = q *> quotedText <* q
  where
    quotedText = (S.concat . reverse) <$> f []

    f soFar = do
        t <- takeWhile qdtext

        let soFar' = t:soFar

        -- RFC says that backslash only escapes for <">
        choice [ string "\\\"" *> f ("\"" : soFar')
               , pure soFar' ]


    q = word8 $ c2w '\"'

    qdtext = matchAll [ isRFCText, (/= '\"'), (/= '\\') ] . w2c
    

pCookies :: Parser [Cookie]
pCookies = do
    -- grab kvps and turn to strict bytestrings
    kvps <- pAvPairs

    return $ map toCookie $ filter (not . S.isPrefixOf "$" . fst) kvps

  where
    toCookie (nm,val) = Cookie nm val Nothing Nothing Nothing


-- unhelpfully, the spec mentions "old-style" cookies that don't have quotes
-- around the value. wonderful.
pWord :: Parser ByteString
pWord = pQuotedString <|> (takeWhile ((/= ';') . w2c))

pAvPairs :: Parser [(ByteString, ByteString)]
pAvPairs = do
    a <- pAvPair
    b <- many (pSpaces *> char ';' *> pSpaces *> pAvPair)

    return $ a:b

pAvPair :: Parser (ByteString, ByteString)
pAvPair = do
    key <- pToken <* pSpaces
    val <- option "" $ char '=' *> pSpaces *> pWord

    return (key,val)

parseCookie :: ByteString -> Maybe [Cookie]
parseCookie = parseToCompletion pCookies

------------------------------------------------------------------------------
-- MULTIPART/FORMDATA
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


------------------------------------------------------------------------------
-- utility functions
------------------------------------------------------------------------------

strictize :: L.ByteString -> ByteString
strictize         = S.concat . L.toChunks

------------------------------------------------------------------------------
char :: Char -> Parser Word8
char = word8 . c2w

