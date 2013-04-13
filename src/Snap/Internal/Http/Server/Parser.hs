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
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder         (Builder)
import           Blaze.ByteString.Builder.HTTP    (chunkedTransferEncoding,
                                                   chunkedTransferTerminator)
import           Control.Exception                (Exception, throwIO)
import qualified Control.Exception                as E
import           Control.Monad                    (void, when)
import           Data.Attoparsec.ByteString.Char8 (Parser, hexadecimal, take,
                                                   takeTill)
import qualified Data.ByteString.Char8            as S
import           Data.ByteString.Internal         (ByteString, w2c)
import qualified Data.ByteString.Unsafe           as S
import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef)
import           Data.Typeable                    (Typeable)
import           GHC.Exts                         (Int (..), Int#, (+#))
import           Prelude                          hiding (head, take,
                                                   takeWhile)
import           System.IO.Streams                (InputStream, OutputStream)
import qualified System.IO.Streams                as Streams
import           System.IO.Streams.Attoparsec     (parseFromStream)
----------------------------------------------------------------------------
import           Snap.Internal.Http.Types         (Method (..))
import           Snap.Internal.Parsing            (crlf, parseCookie,
                                                   parseUrlEncoded,
                                                   unsafeFromNat)


------------------------------------------------------------------------------
-- | an internal version of the headers part of an HTTP request
data IRequest = IRequest
    { iMethod         :: !Method
    , iRequestUri     :: {-# UNPACK #-} !ByteString
    , iHttpVersion    :: {-# UNPACK #-} !(Int, Int)
    , iHost           :: !(Maybe ByteString)
    , iRequestHeaders :: ![(ByteString, ByteString)]
    }
  deriving (Eq)

------------------------------------------------------------------------------
instance Show IRequest where
    show (IRequest m u (major, minor) host r) =
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
        let (!mStr,!s)      = bSp line
        let (!uri, !vStr)   = bSp s
        let method          = methodFromString mStr
        let !version        = pVer vStr
        let (host, uri')    = getHost uri

        hdrs    <- pHeaders input
        return $! Just $! IRequest method uri' version host hdrs

  where
    getHost s | "http://" `S.isPrefixOf` s
                  = let s'            = S.unsafeDrop 7 s
                        (!host, !uri) = breakCh '/' s'
                    in (Just host, uri)
              | "https://" `S.isPrefixOf` s
                  = let s'            = S.unsafeDrop 8 s
                        (!host, !uri) = breakCh '/' s'
                    in (Just host, uri)
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

        case findCRLF s of
            FoundCRLF idx# -> foundCRLF l s idx#
            NoCR           -> noCRLF l s
            LastIsCR idx#  -> lastIsCR l s idx#
            _              -> throwBadCRLF

    foundCRLF l s idx# = do
        let !i1 = (I# idx#)
        let !i2 = (I# (idx# +# 2#))
        let !a = S.unsafeTake i1 s
        when (i2 < S.length s) $ do
            let !b = S.unsafeDrop i2 s
            Streams.unRead b input

        -- Optimize for the common case: dl is almost always "id"
        let !out = if null l then a else S.concat (reverse (a:l))
        return out

    noCRLF l s = go (s:l)

    lastIsCR l s idx# = do
        !t <- Streams.read input >>= maybe throwNoCRLF return
        if S.null t
          then lastIsCR l s idx#
          else do
            let !c = S.unsafeHead t
            if c /= 10
              then throwBadCRLF
              else do
                  let !a = S.unsafeTake (I# idx#) s
                  let !b = S.unsafeDrop 1 t
                  when (not $ S.null b) $ Streams.unRead b input
                  let !out = if null l then a else S.concat (reverse (a:l))
                  return out


------------------------------------------------------------------------------
data CS = FoundCRLF !Int#
        | NoCR
        | LastIsCR !Int#
        | BadCR


------------------------------------------------------------------------------
findCRLF :: ByteString -> CS
findCRLF b =
    case S.elemIndex '\r' b of
      Nothing         -> NoCR
      Just !i@(I# i#) ->
          let !i' = i + 1
          in if i' < S.length b
               then if S.unsafeIndex b i' == 10
                      then FoundCRLF i#
                      else BadCR
               else LastIsCR i#
{-# INLINE findCRLF #-}


------------------------------------------------------------------------------
splitCh :: Char -> ByteString -> (ByteString, ByteString)
splitCh !c !s = maybe (s, S.empty) f (S.elemIndex c s)
  where
    f !i = let !a = S.unsafeTake i s
               !b = S.unsafeDrop (i + 1) s
           in (a, b)
{-# INLINE splitCh #-}


------------------------------------------------------------------------------
breakCh :: Char -> ByteString -> (ByteString, ByteString)
breakCh !c !s = maybe (s, S.empty) f (S.elemIndex c s)
  where
    f !i = let !a = S.unsafeTake i s
               !b = S.unsafeDrop i s
           in (a, b)
{-# INLINE breakCh #-}


------------------------------------------------------------------------------
splitHeader :: ByteString -> (ByteString, ByteString)
splitHeader !s = maybe (s, S.empty) f (S.elemIndex ':' s)
  where
    l = S.length s

    f i = let !a = S.unsafeTake i s
          in (a, skipSp (i + 1))

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
pHeaders :: InputStream ByteString -> IO [(ByteString,ByteString)]
pHeaders input = do
    f <- go id
    return $! f []

  where
    go !dlistSoFar = do
        line <- pLine input
        if S.null line
          then return dlistSoFar
          else do
            let (!k,!v) = splitHeader line
            vf <- pCont id
            let vs = vf []
            let !v' = if null vs then v else S.concat (v:vs)
            let !t = (k,v')
            go (dlistSoFar . (t:))

      where
        trimBegin = S.dropWhile isLWS

        pCont !dlist = do
            mbS  <- Streams.peek input
            maybe (return dlist)
                  (\s -> if S.null s
                           then Streams.read input >> pCont dlist
                           else if isLWS $ w2c $ S.unsafeHead s
                                  then procCont dlist
                                  else return dlist)
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
    void (takeTill (== '\r'))
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
