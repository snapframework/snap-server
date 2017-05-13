{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE Trustworthy        #-}
{-# LANGUAGE UnboxedTuples      #-}

module Snap.Internal.Http.Server.Parser
  ( IRequest(..)
  , HttpParseException(..)
  , readChunkedTransferEncoding
  , writeChunkedTransferEncoding
  , parseRequest
  , parseFromStream
  , parseCookie
  , parseUrlEncoded
  , getStdContentLength
  , getStdHost
  , getStdTransferEncoding
  , getStdCookie
  , getStdContentType
  , getStdConnection
  ) where

------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative              ((<$>))
#endif
import           Control.Exception                (Exception, throwIO)
import qualified Control.Exception                as E
import           Control.Monad                    (void, when)
import           Data.Attoparsec.ByteString.Char8 (Parser, hexadecimal, skipWhile, take, (<?>))
import qualified Data.ByteString.Char8            as S
import           Data.ByteString.Internal         (ByteString (..), c2w, memchr, w2c)
#if MIN_VERSION_bytestring(0, 10, 6)
import           Data.ByteString.Internal         (accursedUnutterablePerformIO)
#else
import           Data.ByteString.Internal         (inlinePerformIO)
#endif
import qualified Data.ByteString.Unsafe           as S
#if !MIN_VERSION_io_streams(1,2,0)
import           Data.IORef                       (newIORef, readIORef, writeIORef)
#endif
import           Data.List                        (sort)
import           Data.Typeable                    (Typeable)
import qualified Data.Vector                      as V
import qualified Data.Vector.Mutable              as MV
import           Foreign.ForeignPtr               (withForeignPtr)
import           Foreign.Ptr                      (minusPtr, nullPtr, plusPtr)
import           Prelude                          hiding (take)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.HTTP    (chunkedTransferEncoding, chunkedTransferTerminator)
import           Data.ByteString.Builder          (Builder)
import           System.IO.Streams                (InputStream, OutputStream)
import qualified System.IO.Streams                as Streams
import           System.IO.Streams.Attoparsec     (parseFromStream)
------------------------------------------------------------------------------
import           Snap.Internal.Http.Types         (Method (..))
import           Snap.Internal.Parsing            (crlf, parseCookie, parseUrlEncoded, unsafeFromNat)
import           Snap.Types.Headers               (Headers)
import qualified Snap.Types.Headers               as H


------------------------------------------------------------------------------
newtype StandardHeaders = StandardHeaders (V.Vector (Maybe ByteString))
type MStandardHeaders = MV.IOVector (Maybe ByteString)


------------------------------------------------------------------------------
contentLengthTag, hostTag, transferEncodingTag, cookieTag, contentTypeTag,
  connectionTag, nStandardHeaders :: Int
contentLengthTag    = 0
hostTag             = 1
transferEncodingTag = 2
cookieTag           = 3
contentTypeTag      = 4
connectionTag       = 5
nStandardHeaders    = 6


------------------------------------------------------------------------------
findStdHeaderIndex :: ByteString -> Int
findStdHeaderIndex "content-length"    = contentLengthTag
findStdHeaderIndex "host"              = hostTag
findStdHeaderIndex "transfer-encoding" = transferEncodingTag
findStdHeaderIndex "cookie"            = cookieTag
findStdHeaderIndex "content-type"      = contentTypeTag
findStdHeaderIndex "connection"        = connectionTag
findStdHeaderIndex _                   = -1


------------------------------------------------------------------------------
getStdContentLength, getStdHost, getStdTransferEncoding, getStdCookie,
    getStdConnection, getStdContentType :: StandardHeaders -> Maybe ByteString
getStdContentLength    (StandardHeaders v) = V.unsafeIndex v contentLengthTag
getStdHost             (StandardHeaders v) = V.unsafeIndex v hostTag
getStdTransferEncoding (StandardHeaders v) = V.unsafeIndex v transferEncodingTag
getStdCookie           (StandardHeaders v) = V.unsafeIndex v cookieTag
getStdContentType      (StandardHeaders v) = V.unsafeIndex v contentTypeTag
getStdConnection       (StandardHeaders v) = V.unsafeIndex v connectionTag


------------------------------------------------------------------------------
newMStandardHeaders :: IO MStandardHeaders
newMStandardHeaders = MV.replicate nStandardHeaders Nothing


------------------------------------------------------------------------------
-- | an internal version of the headers part of an HTTP request
data IRequest = IRequest
    { iMethod         :: !Method
    , iRequestUri     :: !ByteString
    , iHttpVersion    :: (Int, Int)
    , iRequestHeaders :: Headers
    , iStdHeaders     :: StandardHeaders
    }

------------------------------------------------------------------------------
instance Eq IRequest where
    a == b =
        and [ iMethod a      == iMethod b
            , iRequestUri a  == iRequestUri b
            , iHttpVersion a == iHttpVersion b
            , sort (H.toList (iRequestHeaders a))
                  == sort (H.toList (iRequestHeaders b))
            ]

------------------------------------------------------------------------------
instance Show IRequest where
    show (IRequest m u (major, minor) hdrs _) =
        concat [ show m
               , " "
               , show u
               , " "
               , show major
               , "."
               , show minor
               , " "
               , show hdrs
               ]


------------------------------------------------------------------------------
data HttpParseException = HttpParseException String deriving (Typeable, Show)
instance Exception HttpParseException


------------------------------------------------------------------------------
{-# INLINE parseRequest #-}
parseRequest :: InputStream ByteString -> IO IRequest
parseRequest input = do
    line <- pLine input
    let (!mStr, !s)     = bSp line
    let (!uri, !vStr)   = bSp s
    let method          = methodFromString mStr
    let !version        = pVer vStr
    let (host, uri')    = getHost uri
    let uri''           = if S.null uri' then "/" else uri'

    stdHdrs <- newMStandardHeaders
    MV.unsafeWrite stdHdrs hostTag host
    hdrs    <- pHeaders stdHdrs input
    outStd  <- StandardHeaders <$> V.unsafeFreeze stdHdrs
    return $! IRequest method uri'' version hdrs outStd

  where
    getHost s | "http://" `S.isPrefixOf` s
                  = let s'            = S.unsafeDrop 7 s
                        (!host, !uri) = breakCh '/' s'
                    in (Just $! host, uri)
              | "https://" `S.isPrefixOf` s
                  = let s'            = S.unsafeDrop 8 s
                        (!host, !uri) = breakCh '/' s'
                    in (Just $! host, uri)
              | otherwise = (Nothing, s)

    pVer s = if "HTTP/" `S.isPrefixOf` s
               then pVers (S.unsafeDrop 5 s)
               else (1, 0)

    bSp   = splitCh ' '

    pVers s = (c, d)
      where
        (!a, !b)   = splitCh '.' s
        !c         = unsafeFromNat a
        !d         = unsafeFromNat b


------------------------------------------------------------------------------
pLine :: InputStream ByteString -> IO ByteString
pLine input = go []
  where
    throwNoCRLF =
        throwIO $
        HttpParseException "parse error: expected line ending in crlf"

    throwBadCRLF =
        throwIO $
        HttpParseException "parse error: got cr without subsequent lf"

    go !l = do
        !mb <- Streams.read input
        !s  <- maybe throwNoCRLF return mb

        let !i = elemIndex '\r' s
        if i < 0
          then noCRLF l s
          else case () of
                 !_ | i+1 >= S.length s           -> lastIsCR l s i
                    | S.unsafeIndex s (i+1) == 10 -> foundCRLF l s i
                    | otherwise                   -> throwBadCRLF

    foundCRLF l s !i1 = do
        let !i2 = i1 + 2
        let !a = S.unsafeTake i1 s
        when (i2 < S.length s) $ do
            let !b = S.unsafeDrop i2 s
            Streams.unRead b input

        -- Optimize for the common case: dl is almost always "id"
        let !out = if null l then a else S.concat (reverse (a:l))
        return out

    noCRLF l s = go (s:l)

    lastIsCR l s !idx = do
        !t <- Streams.read input >>= maybe throwNoCRLF return
        if S.null t
          then lastIsCR l s idx
          else do
            let !c = S.unsafeHead t
            if c /= 10
              then throwBadCRLF
              else do
                  let !a = S.unsafeTake idx s
                  let !b = S.unsafeDrop 1 t
                  when (not $ S.null b) $ Streams.unRead b input
                  let !out = if null l then a else S.concat (reverse (a:l))
                  return out


------------------------------------------------------------------------------
splitCh :: Char -> ByteString -> (ByteString, ByteString)
splitCh !c !s = if idx < 0
                  then (s, S.empty)
                  else let !a = S.unsafeTake idx s
                           !b = S.unsafeDrop (idx + 1) s
                       in (a, b)
  where
    !idx = elemIndex c s
{-# INLINE splitCh #-}


------------------------------------------------------------------------------
breakCh :: Char -> ByteString -> (ByteString, ByteString)
breakCh !c !s = if idx < 0
                  then (s, S.empty)
                  else let !a = S.unsafeTake idx s
                           !b = S.unsafeDrop idx s
                       in (a, b)
  where
    !idx = elemIndex c s
{-# INLINE breakCh #-}


------------------------------------------------------------------------------
splitHeader :: ByteString -> (ByteString, ByteString)
splitHeader !s = if idx < 0
                   then (s, S.empty)
                   else let !a = S.unsafeTake idx s
                        in (a, skipSp (idx + 1))
  where
    !idx = elemIndex ':' s
    l    = S.length s

    skipSp !i | i >= l    = S.empty
              | otherwise = let c = S.unsafeIndex s i
                            in if isLWS $ w2c c
                                 then skipSp $ i + 1
                                 else S.unsafeDrop i s

{-# INLINE splitHeader #-}



------------------------------------------------------------------------------
isLWS :: Char -> Bool
isLWS c = c == ' ' || c == '\t'
{-# INLINE isLWS #-}


------------------------------------------------------------------------------
pHeaders :: MStandardHeaders -> InputStream ByteString -> IO Headers
pHeaders stdHdrs input = do
    hdrs    <- H.unsafeFromCaseFoldedList <$> go []
    return hdrs

  where
    go !list = do
        line <- pLine input
        if S.null line
          then return list
          else do
            let (!k0,!v) = splitHeader line
            let !k = toLower k0
            vf <- pCont id
            let vs = vf []
            let !v' = S.concat (v:vs)
            let idx = findStdHeaderIndex k
            when (idx >= 0) $ MV.unsafeWrite stdHdrs idx $! Just v'

            let l' = ((k, v'):list)
            go l'

    trimBegin = S.dropWhile isLWS

    pCont !dlist = do
        mbS  <- Streams.peek input
        maybe (return dlist)
              (\s -> if not (S.null s)
                       then if not $ isLWS $ w2c $ S.unsafeHead s
                              then return dlist
                              else procCont dlist
                       else Streams.read input >> pCont dlist)
              mbS

    procCont !dlist = do
        line <- pLine input
        let !t = trimBegin line
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


------------------------------------------------------------------------------
writeChunkedTransferEncoding :: OutputStream Builder
                             -> IO (OutputStream Builder)
#if MIN_VERSION_io_streams(1,2,0)
writeChunkedTransferEncoding os = Streams.makeOutputStream f
  where
    f Nothing = do
        Streams.write (Just chunkedTransferTerminator) os
        Streams.write Nothing os
    f x = Streams.write (chunkedTransferEncoding `fmap` x) os

#else
writeChunkedTransferEncoding os = do
    -- make sure we only send the terminator once.
    eof <- newIORef True
    Streams.makeOutputStream $ f eof
  where
    f eof Nothing = readIORef eof >>= flip when (do
        writeIORef eof True
        Streams.write (Just chunkedTransferTerminator) os
        Streams.write Nothing os)
    f _ x = Streams.write (chunkedTransferEncoding `fmap` x) os
#endif


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
pGetTransferChunk = parser <?> "pGetTransferChunk"
  where
    parser = do
        !hex <- hexadecimal <?> "hexadecimal"
        skipWhile (/= '\r') <?> "skipToEOL"
        void crlf <?> "linefeed"
        if hex >= mAX_CHUNK_SIZE
          then return $! E.throw $! HttpParseException $!
               "pGetTransferChunk: chunk of size " ++ show hex ++ " too long."
          else if hex <= 0
            then (crlf >> return Nothing) <?> "terminal crlf after 0 length"
            else do
                -- now safe to take this many bytes.
                !x <- take hex <?> "reading data chunk"
                void crlf <?> "linefeed after data chunk"
                return $! Just x


------------------------------------------------------------------------------
toLower :: ByteString -> ByteString
toLower = S.map lower
  where
    lower c0 = let !c = c2w c0
               in if 65 <= c && c <= 90
                    then w2c $! c + 32
                    else c0


------------------------------------------------------------------------------
-- | A version of elemIndex that doesn't allocate a Maybe. (It returns -1 on
-- not found.)
elemIndex :: Char -> ByteString -> Int
#if MIN_VERSION_bytestring(0, 10, 6)
elemIndex c (PS !fp !start !len) = accursedUnutterablePerformIO $
#else
elemIndex c (PS !fp !start !len) = inlinePerformIO $
#endif
                                   withForeignPtr fp $ \p0 -> do
    let !p = plusPtr p0 start
    q <- memchr p w8 (fromIntegral len)
    return $! if q == nullPtr then (-1) else q `minusPtr` p
  where
    !w8 = c2w c
{-# INLINE elemIndex #-}
