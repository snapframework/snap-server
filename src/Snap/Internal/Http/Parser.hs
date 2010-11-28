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
  , writeChunkedTransferEncoding
  , strictize
  ) where


------------------------------------------------------------------------------
import             Control.Applicative
import             Control.Arrow (second)
import             Control.Monad (liftM)
import "monads-fd" Control.Monad.Trans
import             Data.Attoparsec hiding (many, Result(..))
import             Data.Attoparsec.Enumerator
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
import             Data.Map (Map)
import qualified   Data.Map as Map
import             Data.Maybe (catMaybes)
import qualified   Data.Vector.Unboxed as Vec
import             Data.Vector.Unboxed (Vector)
import             Data.Word (Word8, Word64)
import             Prelude hiding (head, take, takeWhile)
import qualified   Prelude
------------------------------------------------------------------------------
import             Snap.Internal.Http.Types
import             Snap.Internal.Debug
import             Snap.Internal.Iteratee.Debug
import             Snap.Iteratee hiding (map, take)


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
-- > FIXME this text is now wrong
--
-- > > (writeChunkedTransferEncoding
-- >     (enumLBS (L.fromChunks ["foo","bar","quux"]))
-- >     stream2stream) >>=
-- >     run >>=
-- >     return . fromWrap
-- >
-- > Chunk "a\r\nfoobarquux\r\n0\r\n\r\n" Empty
--
writeChunkedTransferEncoding :: Enumeratee ByteString ByteString IO a
writeChunkedTransferEncoding = checkDone start

  where
    start = bufIt 0 D.empty

    bufSiz = 16284

    sendOut :: DList ByteString
            -> (Stream ByteString -> Iteratee ByteString IO a)
            -> Iteratee ByteString IO (Step ByteString IO a)
    sendOut dl k = do
        let chunks = D.toList dl
        let bs     = L.fromChunks chunks
        let n      = L.length bs

        if n == 0
          then return $ Continue k
          else do
            let o = L.concat [ L.fromChunks [ toHex (toEnum . fromEnum $ n)
                                            , "\r\n" ]
                             , bs
                             , "\r\n" ]

            lift $ runIteratee $ enumLBS o (Continue k)


    
    bufIt :: Int
          -> DList ByteString
          -> (Stream ByteString -> Iteratee ByteString IO a)
          -> Iteratee ByteString IO (Step ByteString IO a)
    bufIt n dl k = do
        mbS <- head
        case mbS of
          Nothing -> do
              step  <- sendOut dl k
              step' <- lift $ runIteratee $ enumBS "0\r\n\r\n" step
              lift $ runIteratee $ enumEOF step'

          (Just s) -> do
              let m = S.length s

              if m == 0
                then bufIt n dl k
                else do
                  let n'  = m + n
                  let dl' = D.snoc dl s

                  if n' > bufSiz
                    then do
                      step <- sendOut dl' k
                      checkDone start step
                    else bufIt n' dl' k


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
