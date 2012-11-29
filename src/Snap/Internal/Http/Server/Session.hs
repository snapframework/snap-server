{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

------------------------------------------------------------------------------
module Snap.Internal.Http.Server.Session
  ( httpSession
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder         (flush, fromByteString)
import           Blaze.ByteString.Builder.Char8   (fromChar, fromShow)
import           Control.Applicative              ((<$>), (<|>))
import           Control.Arrow                    (first, second)
import           Control.Exception                (Exception,
                                                   SomeException (..), throwIO)
import           Control.Monad                    (when)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as S
import qualified Data.CaseInsensitive             as CI
import           Data.Int                         (Int64)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      (mconcat)
import           Data.Typeable                    (Typeable)
import           System.IO.Streams                (InputStream)
import qualified System.IO.Streams                as Streams
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Parser (IRequest (..), parseCookie,
                                                   parseRequest,
                                                   parseUrlEncoded,
                                                   readChunkedTransferEncoding)
import           Snap.Internal.Http.Server.Types  (AcceptHook, DataFinishedHook,
                                                   ParseHook,
                                                   PerSessionData (..),
                                                   ServerConfig (..),
                                                   ServerHandler,
                                                   SessionFinishedHook,
                                                   SessionHandler,
                                                   UserHandlerFinishedHook)
import           Snap.Internal.Http.Types         (Method (..), Request (..))
import           Snap.Internal.Parsing            (unsafeFromInt)
import qualified Snap.Types.Headers               as H
------------------------------------------------------------------------------


------------------------------------------------------------------------------
data TerminateSessionException = TerminateSessionException SomeException
  deriving (Typeable, Show)
instance Exception TerminateSessionException

data BadRequestException = BadRequestException
  deriving (Typeable, Show)
instance Exception BadRequestException

data LengthRequiredException = LengthRequiredException
  deriving (Typeable, Show)
instance Exception LengthRequiredException


------------------------------------------------------------------------------
mAX_HEADERS_SIZE :: Int64
mAX_HEADERS_SIZE = 256 * 1024


------------------------------------------------------------------------------
httpSession :: ServerHandler hookState
            -> ServerConfig hookState
            -> PerSessionData
            -> IO ()
httpSession !serverHandler !config !sessionData = begin
  where
    --------------------------------------------------------------------------
    defaultTimeout = _defaultTimeout config
    localAddress   = _localAddress sessionData
    localPort      = _localPort config
    localHostname  = _localHostname config
    readEnd        = _readEnd sessionData
    remoteAddress  = _remoteAddress sessionData
    remotePort     = _remotePort sessionData
    tickle         = _twiddleTimeout sessionData
    writeEnd       = _writeEnd sessionData
    isSecure       = _isSecure config

    --------------------------------------------------------------------------
    {-# INLINE begin #-}
    begin :: IO ()
    begin = do
        -- call "accept hook"
        hookState <- _onAccept config sessionData
        -- parse HTTP request
        receiveRequest >>= maybe (_onSessionFinished config hookState)
                                 (processRequest hookState)

    --------------------------------------------------------------------------
    {-# INLINE receiveRequest #-}
    receiveRequest :: IO (Maybe Request)
    receiveRequest = do
        readEnd' <- Streams.throwIfProducesMoreThan mAX_HEADERS_SIZE readEnd
        parseRequest readEnd' >>= maybe (return Nothing)
                                        ((Just <$>) . toRequest)

    --------------------------------------------------------------------------
    toRequest :: IRequest -> IO Request
    toRequest !ireq = do
        -- HTTP spec section 14.23: "All Internet-based HTTP/1.1 servers MUST
        -- respond with a 400 (Bad Request) status code to any HTTP/1.1 request
        -- message which lacks a Host header field."
        --
        -- Here we interpret this slightly more liberally: if an absolute URI
        -- is given in the request line, we'll take that if there's no Host
        -- header.
        --
        -- For HTTP/1.0 requests, we pick the configured local hostname by
        -- default.
        host <- maybe (if isHttp11
                         then badRequestWithNoHost
                         else return localHostname)
                      return mbHost

        -- Handle transfer-encoding: chunked, etc
        readEnd' <- setupReadEnd
        (readEnd'', postParams) <- parseForm readEnd'
        let allParams = Map.unionWith (++) queryParams postParams
        return $! Request host
                          remoteAddress
                          remotePort
                          localAddress
                          localPort
                          localHostname
                          isSecure
                          hdrs
                          readEnd''
                          mbCL
                          method
                          version
                          cookies
                          pathInfo
                          contextPath
                          uri
                          queryString
                          allParams
                          queryParams
                          postParams

      where
        ----------------------------------------------------------------------
        method      = iMethod ireq
        version     = iHttpVersion ireq
        isHttp11    = version >= (1, 1)
        mbHost      = H.lookup "host" hdrs <|> iHost ireq
        localHost   = fromMaybe localHostname $! iHost ireq
        hdrs        = toHeaders $! iRequestHeaders ireq
        isChunked   = (CI.mk <$> H.lookup "transfer-encoding" hdrs)
                         == Just "chunked"
        mbCL        = unsafeFromInt <$> H.lookup "content-length" hdrs
        cookies     = fromMaybe [] (H.lookup "cookie" hdrs >>= parseCookie)
        contextPath = "/"
        uri         = iRequestUri ireq
        queryParams = parseUrlEncoded queryString
        emptyParams = Map.empty

        ----------------------------------------------------------------------
        (pathInfo, queryString) = first dropLeadingSlash . second (S.drop 1)
                                    $ S.break (== '?') uri

        ----------------------------------------------------------------------
        {-# INLINE dropLeadingSlash #-}
        dropLeadingSlash s = let f (a, s') = if a == '/' then s' else s
                                 mbS       = S.uncons s
                             in maybe s f mbS

        ----------------------------------------------------------------------
        {-# INLINE setupReadEnd #-}
        setupReadEnd = do
            readEnd' <- if isChunked
                          then readChunkedTransferEncoding readEnd
                          else return readEnd
            maybe noContentLength Streams.takeBytes mbCL $ readEnd'

        ----------------------------------------------------------------------
        noContentLength :: InputStream ByteString
                        -> IO (InputStream ByteString)
        noContentLength readEnd' = do
            when (method `elem` [POST, PUT]) return411
            Streams.takeBytes 0 readEnd'

        ----------------------------------------------------------------------
        return411 = do
            let (major, minor) = version
            let resp = mconcat [ fromByteString "HTTP/"
                               , fromShow major
                               , fromChar '.'
                               , fromShow minor
                               , fromByteString " 411 Length Required\r\n\r\n"
                               , fromByteString "411 Length Required\r\n"
                               , flush
                               ]
            Streams.write (Just resp) writeEnd
            Streams.write Nothing writeEnd
            terminateSession LengthRequiredException

        ----------------------------------------------------------------------
        parseForm readEnd' = if doIt
                               then getIt
                               else return (readEnd', emptyParams)
          where
            trimIt = fst . S.spanEnd (== ' ') . S.takeWhile (/= ';')
                         . S.dropWhile (== ' ')
            mbCT   = trimIt <$> H.lookup "content-type" hdrs
            doIt   = mbCT == Just "application/x-www-form-urlencoded"

            mAX_POST_BODY_SIZE = 1024 * 1024

            getIt = do
                readEnd'' <- Streams.throwIfProducesMoreThan
                               mAX_POST_BODY_SIZE readEnd'
                contents  <- S.concat <$> Streams.toList readEnd''
                let postParams = parseUrlEncoded contents
                finalReadEnd <- Streams.fromList [contents]
                return (finalReadEnd, postParams)

    --------------------------------------------------------------------------
    {-# INLINE badRequestWithNoHost #-}
    badRequestWithNoHost :: IO a
    badRequestWithNoHost = do
        let msg = mconcat [
                    fromByteString "HTTP/1.1 400 Bad Request\r\n\r\n"
                  , fromByteString "Bad request: no Host header"
                  , flush
                  ]
        Streams.write (Just msg) writeEnd
        Streams.write Nothing writeEnd
        terminateSession BadRequestException

    --------------------------------------------------------------------------
    {-# INLINE processRequest #-}
    processRequest !hookState !ireq = do
        -- successfully parsed a request, so restart the timer
        tickle $ max defaultTimeout

        -- check for Expect: 100-continue
        undefined


------------------------------------------------------------------------------
toHeaders :: [(ByteString, ByteString)] -> H.Headers
toHeaders = H.fromList . map (first CI.mk)


------------------------------------------------------------------------------
terminateSession :: Exception e => e -> IO a
terminateSession = throwIO . TerminateSessionException . SomeException
