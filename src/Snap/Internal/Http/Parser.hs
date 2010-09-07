{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
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
import             Control.Applicative
import             Control.Arrow (second)
import             Control.Monad (liftM)
import "monads-fd" Control.Monad.Trans
import             Data.Attoparsec hiding (many, Result(..))
import             Data.Attoparsec.Iteratee
import             Data.Bits
import             Data.ByteString (ByteString)
import qualified   Data.ByteString as S
import             Data.ByteString.Internal (c2w, w2c)
import qualified   Data.ByteString.Lazy as L
import qualified   Data.ByteString.Nums.Careless.Hex as Cvt
import             Data.Char
import             Data.DList (DList)
import qualified   Data.DList as D
import             Data.List (foldl')
import             Data.Int
import             Data.Iteratee.WrappedByteString
import             Data.Map (Map)
import qualified   Data.Map as Map
import             Data.Maybe (catMaybes)
import qualified   Data.Vector.Unboxed as Vec
import             Data.Vector.Unboxed (Vector)
import             Data.Word (Word8, Word64)
import             Prelude hiding (take, takeWhile)
------------------------------------------------------------------------------
import             Snap.Internal.Http.Types hiding (Enumerator)
import             Snap.Iteratee hiding (take, foldl', filter)


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
parseRequest :: (Monad m) => Iteratee m (Maybe IRequest)
parseRequest = parserToIteratee pRequest


------------------------------------------------------------------------------
readChunkedTransferEncoding :: (Monad m) =>
                               Iteratee m a
                            -> m (Iteratee m a)
readChunkedTransferEncoding iter = do
    i <- chunkParserToEnumerator (parserToIteratee pGetTransferChunk)
                                 iter
    return i


------------------------------------------------------------------------------
toHex :: Int64 -> S.ByteString
toHex 0 = "0"
toHex n' = s
  where
    !s = trim 16 (fromIntegral (abs n'))

    trim :: Int -> Word64 -> S.ByteString
    trim !i !n
      | n .&. 0xf000000000000000 == 0 = trim (i-1) (n `shiftL` 4)
      | otherwise = fst (S.unfoldrN i f n)

    f n = Just (ch (n `shiftR` 60), n `shiftL` 4)

    ch (fromIntegral -> i)
      | i < 10    = (c2w '0' -  0) + i
      | otherwise = (c2w 'a' - 10) + i


------------------------------------------------------------------------------
-- | Given an iteratee, produces a new one that wraps chunks sent to it with a
-- chunked transfer-encoding. Example usage:
--
--
-- > > (writeChunkedTransferEncoding
-- >     (enumLBS (L.fromChunks ["foo","bar","quux"]))
-- >     stream2stream) >>=
-- >     run >>=
-- >     return . fromWrap
-- >
-- > Chunk "a\r\nfoobarquux\r\n0\r\n\r\n" Empty
--
writeChunkedTransferEncoding :: Enumerator IO a
writeChunkedTransferEncoding it = do
    let out = wrap it
    return out

  where
    wrap iter = bufIt (0,D.empty) iter

    bufSiz = 16284

    sendOut :: DList ByteString
            -> Iteratee IO a
            -> IO (Iteratee IO a)
    sendOut dl iter = do
        let chunks = D.toList dl
        let bs     = L.fromChunks chunks
        let n      = L.length bs

        if n == 0
          then return iter
          else do
            let o = L.concat [ L.fromChunks [ toHex (toEnum . fromEnum $ n)
                                            , "\r\n" ]
                             , bs
                             , "\r\n" ]

            enumLBS o iter


    bufIt (n,dl) iter = IterateeG $ \s -> do
        case s of
          (EOF Nothing) -> do
               i'  <- sendOut dl iter
               j   <- liftM liftI $ runIter i' (Chunk (WrapBS "0\r\n\r\n"))
               runIter j (EOF Nothing)

          (EOF e) -> return $ Cont undefined e

          (Chunk (WrapBS x)) -> do
               let m   = S.length x

               if m == 0
                 then return $ Cont (bufIt (n,dl) iter) Nothing
                 else do
                   let n'  = m + n
                   let dl' = D.snoc dl x

                   if n' > bufSiz
                     then do
                       i' <- sendOut dl' iter
                       return $ Cont (bufIt (0,D.empty) i') Nothing
                     else return $ Cont (bufIt (n',dl') iter) Nothing


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
-- | Parsers for different tokens in an HTTP request.
sp, digit, letter :: Parser Word8
sp       = word8 $ c2w ' '
digit    = satisfy (isDigit . w2c)
letter   = satisfy (isAlpha . w2c)


------------------------------------------------------------------------------
untilEOL :: Parser ByteString
untilEOL = takeWhile notend
  where
    notend d = let c = w2c d in not $ c == '\r' || c == '\n'


------------------------------------------------------------------------------
crlf :: Parser ByteString
crlf = string "\r\n"


------------------------------------------------------------------------------
-- | Parser for zero or more spaces.
spaces :: Parser [Word8]
spaces = many sp


------------------------------------------------------------------------------
pSpaces :: Parser ByteString
pSpaces = takeWhile (isSpace . w2c)


------------------------------------------------------------------------------
-- | Parser for the internal request data type.
pRequest :: Parser (Maybe IRequest)
pRequest = (Just <$> pRequest') <|> (endOfInput *> pure Nothing)


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
      digit' = fmap (digitToInt . w2c) digit


------------------------------------------------------------------------------
fieldChars :: Parser ByteString
fieldChars = takeWhile isFieldChar
  where
    isFieldChar c = (Vec.!) fieldCharTable (fromEnum c)


------------------------------------------------------------------------------
fieldCharTable :: Vector Bool
fieldCharTable = Vec.generate 256 f
  where
    f d = let c=toEnum d in (isDigit c) || (isAlpha c) || c == '-' || c == '_'


------------------------------------------------------------------------------
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
{-# INLINE matchAll #-}
matchAll :: [ Char -> Bool ] -> Char -> Bool
matchAll x c = and $ map ($ c) x


------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
{-# INLINE isRFCText #-}
isRFCText :: Char -> Bool
isRFCText = not . isControl


------------------------------------------------------------------------------
pToken :: Parser ByteString
pToken = takeWhile (isToken . w2c)


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
pCookies :: Parser [Cookie]
pCookies = do
    -- grab kvps and turn to strict bytestrings
    kvps <- pAvPairs

    return $ map toCookie $ filter (not . S.isPrefixOf "$" . fst) kvps

  where
    toCookie (nm,val) = Cookie nm val Nothing Nothing Nothing


------------------------------------------------------------------------------
-- unhelpfully, the spec mentions "old-style" cookies that don't have quotes
-- around the value. wonderful.
pWord :: Parser ByteString
pWord = pQuotedString <|> (takeWhile ((/= ';') . w2c))


------------------------------------------------------------------------------
pAvPairs :: Parser [(ByteString, ByteString)]
pAvPairs = do
    a <- pAvPair
    b <- many (pSpaces *> char ';' *> pSpaces *> pAvPair)

    return $ a:b


------------------------------------------------------------------------------
pAvPair :: Parser (ByteString, ByteString)
pAvPair = do
    key <- pToken <* pSpaces
    val <- option "" $ char '=' *> pSpaces *> pWord

    return (key,val)


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


------------------------------------------------------------------------------
-- utility functions
------------------------------------------------------------------------------

------------------------------------------------------------------------------
strictize :: L.ByteString -> ByteString
strictize         = S.concat . L.toChunks


------------------------------------------------------------------------------
char :: Char -> Parser Word8
char = word8 . c2w
