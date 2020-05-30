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
import           Control.Monad                    (void, when)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Attoparsec.ByteString.Char8 (Parser, hexadecimal, takeTill)
import qualified Data.ByteString.Char8            as S
import           Data.ByteString.Internal         (ByteString (..), c2w, memchr, w2c)
#if MIN_VERSION_bytestring(0, 10, 6)
import           Data.ByteString.Internal         (accursedUnutterablePerformIO)
#else
import           Data.ByteString.Internal         (inlinePerformIO)
#endif
import qualified Data.ByteString.Unsafe           as S
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
import           System.IO.Streams                (InputStream, OutputStream, Generator)
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
    Streams.fromGenerator (consumeChunks input)

------------------------------------------------------------------------------
writeChunkedTransferEncoding :: OutputStream Builder
                             -> IO (OutputStream Builder)
writeChunkedTransferEncoding os = Streams.makeOutputStream f
  where
    f Nothing = do
        Streams.write (Just chunkedTransferTerminator) os
        Streams.write Nothing os
    f x = Streams.write (chunkedTransferEncoding `fmap` x) os


                             ---------------------
                             -- parse functions --
                             ---------------------

------------------------------------------------------------------------------
{-
    For a response body in chunked transfer encoding, iterate over
    the individual chunks, reading the size parameter, then
    looping over that chunk in bites of at most bUFSIZ,
    yielding them to the receiveResponse InputStream accordingly.
-}
consumeChunks :: InputStream ByteString -> Generator ByteString ()
consumeChunks i1 = do
    !n <- parseSize
    if n > 0
        then do
            -- read one or more bytes, then loop to next chunk
            go n
            skipCRLF
            consumeChunks i1
        else do
            -- NB: snap-server doesn't yet support chunked trailer parts
            -- (see RFC7230#sec4.1.2)

            -- consume final CRLF
            skipCRLF

  where
    go 0 = return ()
    go !n = do
        (!x',!r) <- liftIO $ readN n i1
        Streams.yield x'
        go r

    parseSize = do
        liftIO $ parseFromStream transferChunkSize i1

    skipCRLF = do
        liftIO $ void (parseFromStream crlf i1)

    transferChunkSize :: Parser (Int)
    transferChunkSize = do
        !n <- hexadecimal
        -- skip over any chunk extensions (see RFC7230#sec4.1.1)
        void (takeTill (== '\r'))
        void crlf
        return n

    {-
        The chunk size coming down from the client is somewhat arbitrary;
        it's really just an indication of how many bytes need to be read
        before the next size marker or end marker - neither of which has
        anything to do with streaming on our side. Instead, we'll feed
        bytes into our InputStream at an appropriate intermediate size.
    -}
    bUFSIZ :: Int
    bUFSIZ = 32752

    {-
        Read the specified number of bytes up to a maximum of bUFSIZ,
        returning a resultant ByteString and the number of bytes remaining.
    -}
    readN :: Int -> InputStream ByteString -> IO (ByteString, Int)
    readN n input = do
        !x' <- Streams.readExactly p input
        return (x', r)
      where
        !d = n - bUFSIZ
        !p = if d > 0 then bUFSIZ else n
        !r = if d > 0 then d else 0

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
