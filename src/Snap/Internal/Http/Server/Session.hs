{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
module Snap.Internal.Http.Server.Session
  ( httpAcceptLoop
  , httpSession
  , BadRequestException(..)
  , LengthRequiredException(..)
  , TerminateSessionException(..)
  ) where

------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,6,0)
import           Prelude                                  hiding (catch)
#endif
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (Builder, flush,
                                                           fromByteString)
import           Blaze.ByteString.Builder.Char8           (fromChar, fromShow,
                                                           fromString)
import           Blaze.ByteString.Builder.Internal        (defaultBufferSize)
import           Blaze.ByteString.Builder.Internal.Buffer (Buffer,
                                                           allocBuffer)
------------------------------------------------------------------------------
import           Control.Applicative                      ((<$>), (<|>))
import           Control.Arrow                            (first, second)
import           Control.Concurrent                       (MVar, ThreadId,
                                                           forkIOWithUnmask,
                                                           forkOnWithUnmask,
                                                           killThread,
                                                           myThreadId,
                                                           newEmptyMVar,
                                                           putMVar, readMVar,
                                                           takeMVar)
import           Control.Exception                        (AsyncException,
                                                           Exception,
                                                           Handler (..),
                                                           IOException,
                                                           SomeException (..),
                                                           bracket, catch,
                                                           catches, evaluate,
                                                           finally, mask_,
                                                           throwIO)
import           Control.Monad                            (forever, unless,
                                                           when)
import           Data.ByteString.Char8                    (ByteString)
import qualified Data.ByteString.Char8                    as S
import qualified Data.CaseInsensitive                     as CI
import           Data.Int                                 (Int64)
import           Data.IORef                               (IORef, newIORef,
                                                           readIORef,
                                                           writeIORef)
import           Data.List                                (foldl')
import qualified Data.Map                                 as Map
import           Data.Maybe                               (fromJust,
                                                           fromMaybe, isJust,
                                                           isNothing)
import           Data.Monoid                              (mconcat, mempty,
                                                           (<>))
import           Data.Time.Format                         (formatTime)
import           Data.Typeable                            (Typeable)
import           Data.Version                             (showVersion)
import           System.IO.Streams                        (InputStream,
                                                           OutputStream)
import qualified System.IO.Streams                        as Streams
import           System.Locale                            (defaultTimeLocale)
------------------------------------------------------------------------------
import qualified Paths_snap_server                        as V
import           Snap.Core                                (EscapeSnap (..))
import           Snap.Internal.Http.Types                 (Cookie (..),
                                                           Method (..),
                                                           Request (..),
                                                           Response (..),
                                                           ResponseBody (..),
                                                           StreamProc,
                                                           getHeader, headers,
                                                           rspBodyToEnum,
                                                           setContentLength,
                                                           setHeader,
                                                           updateHeaders)
import           Snap.Internal.Parsing                    (unsafeFromNat)
import           Snap.Types.Headers                       (Headers)
import qualified Snap.Types.Headers                       as H
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Common         (eatException)
import           Snap.Internal.Http.Server.Date           (getCurrentDateTime,
                                                           getDateString)
import           Snap.Internal.Http.Server.Parser         (IRequest (..),
                                                           parseCookie,
                                                           parseRequest,
                                                           parseUrlEncoded, readChunkedTransferEncoding, writeChunkedTransferEncoding)
import           Snap.Internal.Http.Server.TimeoutManager (TimeoutManager)
import qualified Snap.Internal.Http.Server.TimeoutManager as TM
import           Snap.Internal.Http.Server.Types          (AcceptFunc, PerSessionData (..),
                                                           SendFileHandler,
                                                           ServerConfig (..),
                                                           ServerHandler)
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
-- | For each cpu, we store:
--    * An accept thread
--    * A TimeoutManager
--    * An mvar to signal when the timeout thread is shutdown
data EventLoopCpu = EventLoopCpu
    { _acceptThread   :: ThreadId
    , _timeoutManager :: TimeoutManager
    , _exitMVar       :: !(MVar ())
    }


------------------------------------------------------------------------------
httpAcceptLoop :: forall hookState .
                  ServerHandler hookState  -- ^ server handler
               -> ServerConfig hookState   -- ^ server config
               -> AcceptFunc               -- ^ accept function
               -> IO ()
httpAcceptLoop serverHandler serverConfig acceptFunc = runLoops
  where
    --------------------------------------------------------------------------
    logError = _logError serverConfig
    nLoops         = _numAcceptLoops serverConfig
    defaultTimeout = _defaultTimeout serverConfig

    --------------------------------------------------------------------------
    logException :: Exception e => e -> IO ()
    logException e =
        logError $
        mconcat [ fromByteString "got exception in httpAcceptFunc: "
                , fromShow e
                ]

    --------------------------------------------------------------------------
    runLoops = bracket (mapM newLoop [0 .. (nLoops - 1)])
                       (mapM_ killLoop)
                       (mapM_ waitLoop)

    --------------------------------------------------------------------------
    loop :: MVar ()
         -> TimeoutManager
         -> (forall a. IO a -> IO a)
         -> IO ()
    loop mv tm loopRestore = eatException go `finally` putMVar mv ()
      where
        ----------------------------------------------------------------------
        handlers = [ Handler $ \(e :: IOException)    -> logException e >> go
                   , Handler $ \(e :: AsyncException) -> throwIO e
                   , Handler $ \(e :: SomeException)  -> logException e >>
                                                         throwIO e
                   ]

        go = forever $ do
            (sendFileHandler, localAddress, remoteAddress, remotePort,
             readEnd, writeEnd, cleanup) <- acceptFunc loopRestore
                                            `catches` handlers

            forkIOWithUnmask
                $ eatException
                . prep sendFileHandler localAddress remoteAddress
                       remotePort readEnd writeEnd cleanup

        prep :: SendFileHandler
             -> ByteString
             -> ByteString
             -> Int
             -> InputStream ByteString
             -> OutputStream ByteString
             -> IO ()
             -> (forall a . IO a -> IO a)
             -> IO ()
        prep sendFileHandler localAddress remoteAddress remotePort readEnd
             writeEnd cleanup restore =
          do
            connClose <- newIORef False
            newConn   <- newIORef True
            tid       <- myThreadId
            tmHandle  <- TM.register (killThread tid) tm
            let twiddleTimeout = TM.modify tmHandle

            let psd = PerSessionData connClose
                                     twiddleTimeout
                                     sendFileHandler
                                     localAddress
                                     remoteAddress
                                     remotePort
                                     readEnd
                                     writeEnd
                                     newConn
            restore (session psd) `finally` cleanup

    --------------------------------------------------------------------------
    session psd = do
        buffer <- allocBuffer defaultBufferSize
        httpSession buffer serverHandler serverConfig psd

    --------------------------------------------------------------------------
    newLoop cpu = mask_ $ do
        mv  <- newEmptyMVar
        tm  <- TM.initialize defaultTimeout getCurrentDateTime
        tid <- forkOnWithUnmask cpu $ loop mv tm
        return $! EventLoopCpu tid tm mv

    --------------------------------------------------------------------------
    waitLoop (EventLoopCpu _ _ mv) = readMVar mv

    --------------------------------------------------------------------------
    killLoop (EventLoopCpu tid tm mv) = mask_ $ do
        killThread tid
        TM.stop tm
        takeMVar mv >>= evaluate


------------------------------------------------------------------------------
httpSession :: forall hookState .
               Buffer
            -> ServerHandler hookState
            -> ServerConfig hookState
            -> PerSessionData
            -> IO ()
httpSession !buffer !serverHandler !config !sessionData =
    begin `finally` cleanup

  where
    --------------------------------------------------------------------------
    defaultTimeout          = _defaultTimeout config
    isSecure                = _isSecure config
    localHostname           = _localHostname config
    localPort               = _localPort config
    logAccess               = _logAccess config
    logError                = _logError config
    startHook               = _onStart config
    parseHook               = _onParse config
    userHandlerFinishedHook = _onUserHandlerFinished config
    dataFinishedHook        = _onDataFinished config
    exceptionHook           = _onException config
    escapeHook              = _onEscape config

    --------------------------------------------------------------------------
    forceConnectionClose    = _forceConnectionClose sessionData
    isNewConnection         = _isNewConnection sessionData
    localAddress            = _localAddress sessionData
    readEnd                 = _readEnd sessionData
    remoteAddress           = _remoteAddress sessionData
    remotePort              = _remotePort sessionData
    tickle                  = _twiddleTimeout sessionData
    writeEnd                = _writeEnd sessionData
    sendfileHandler         = _sendfileHandler sessionData

    --------------------------------------------------------------------------
    newBuffer :: IO (OutputStream Builder)
    newBuffer = Streams.unsafeBuilderStream (return buffer) writeEnd

    --------------------------------------------------------------------------
    -- Begin HTTP session processing.
    begin :: IO ()
    begin = do
        -- the timer resets to its default value here.
        tickle $ const defaultTimeout

        -- peek first to ensure startHook gets generated at the right time.
        readEndAtEof >>= (flip unless $ do
            hookState <- startHook sessionData >>= newIORef
            -- parse HTTP request
            req <- receiveRequest
            parseHook hookState req
            processRequest hookState req)
    {-# INLINE begin #-}

    ------------------------------------------------------------------------------
    cleanup :: IO ()
    cleanup = Streams.write Nothing writeEnd

    ------------------------------------------------------------------------------
    readEndAtEof = Streams.read readEnd >>=
                   maybe (return True)
                         (\c -> if S.null c
                                  then readEndAtEof
                                  else Streams.unRead c readEnd >> return False)

    --------------------------------------------------------------------------
    -- Read the HTTP request from the socket, parse it, and pre-process it.
    receiveRequest :: IO Request
    receiveRequest = do
        readEnd' <- Streams.throwIfProducesMoreThan mAX_HEADERS_SIZE readEnd
        parseRequest readEnd' >>= toRequest
    {-# INLINE receiveRequest #-}

    --------------------------------------------------------------------------
    toRequest :: IRequest -> IO Request
    toRequest !ireq = do
        -- HTTP spec section 14.23: "All Internet-based HTTP/1.1 servers MUST
        -- respond with a 400 (Bad Request) status code to any HTTP/1.1 request
        -- message which lacks a Host header field."
        --
        -- Here we interpret this slightly more liberally: if an absolute URI
        -- including a hostname is given in the request line, we'll take that
        -- if there's no Host header.
        --
        -- For HTTP/1.0 requests, we pick the configured local hostname by
        -- default.
        host <- maybe (if isHttp11
                         then badRequestWithNoHost
                         else return localHostname)
                      return mbHost

        -- Call setupReadEnd, which handles transfer-encoding: chunked or
        -- content-length restrictions, etc
        readEnd' <- setupReadEnd

        -- Parse an application/x-www-form-urlencoded form, if it was sent
        (readEnd'', postParams) <- parseForm readEnd'

        let allParams = Map.unionWith (++) queryParams postParams

        -- Decide whether the connection should be closed after the response is
        -- sent (stored in the forceConnectionClose IORef).
        checkConnectionClose version hdrs

        -- The request is now ready for processing.
        return $! Request host
                          remoteAddress
                          remotePort
                          localAddress
                          localPort
                          localHost
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
        mbCL        = unsafeFromNat <$> H.lookup "content-length" hdrs
        cookies     = fromMaybe [] (H.lookup "cookie" hdrs >>= parseCookie)
        contextPath = "/"
        uri         = iRequestUri ireq
        queryParams = parseUrlEncoded queryString
        emptyParams = Map.empty

        ----------------------------------------------------------------------
        (pathInfo, queryString) = first dropLeadingSlash . second (S.drop 1)
                                    $ S.break (== '?') uri

        ----------------------------------------------------------------------
        dropLeadingSlash s = let f (a, s') = if a == '/' then s' else s
                                 mbS       = S.uncons s
                             in maybe s f mbS
        {-# INLINE dropLeadingSlash #-}

        ----------------------------------------------------------------------
        -- | We have to transform the read end of the socket, to limit the
        -- number of bytes read to the content-length, to decode chunked
        -- transfer encoding, or to immediately yield EOF if the request body
        -- is empty.
        setupReadEnd :: IO (InputStream ByteString)
        setupReadEnd =
            if isChunked
              then readChunkedTransferEncoding readEnd
              else maybe (const noContentLength) Streams.takeBytes mbCL readEnd
        {-# INLINE setupReadEnd #-}

        ----------------------------------------------------------------------
        -- | If a request is not in chunked transfer encoding and lacks a
        -- content-length, the request body is null string.
        noContentLength :: IO (InputStream ByteString)
        noContentLength = do
            when (method `elem` [POST, PUT]) return411
            Streams.fromList []

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
            writeEndB <- newBuffer
            Streams.write (Just resp) writeEndB
            Streams.write Nothing writeEndB
            terminateSession LengthRequiredException

        ----------------------------------------------------------------------
        parseForm readEnd' = if hasForm
                               then getForm
                               else return (readEnd', emptyParams)
          where
            trimIt  = fst . S.spanEnd (== ' ') . S.takeWhile (/= ';')
                          . S.dropWhile (== ' ')
            mbCT    = trimIt <$> H.lookup "content-type" hdrs
            hasForm = mbCT == Just "application/x-www-form-urlencoded"

            mAX_POST_BODY_SIZE = 1024 * 1024

            getForm = do
                readEnd'' <- Streams.throwIfProducesMoreThan
                               mAX_POST_BODY_SIZE readEnd'
                contents  <- S.concat <$> Streams.toList readEnd''
                let postParams = parseUrlEncoded contents
                finalReadEnd <- Streams.fromList [contents]
                return (finalReadEnd, postParams)

    ----------------------------------------------------------------------
    checkConnectionClose version hdrs = do
        -- For HTTP/1.1: if there is an explicit Connection: close, we'll close
        -- the socket later.
        --
        -- For HTTP/1.0: if there is no explicit Connection: Keep-Alive,
        -- close the socket later.
        let v = CI.mk <$> H.lookup "Connection" hdrs
        when ((version == (1, 1) && v == Just "close") ||
              (version == (1, 0) && v /= Just "keep-alive")) $
              writeIORef forceConnectionClose True

    --------------------------------------------------------------------------
    {-# INLINE badRequestWithNoHost #-}
    badRequestWithNoHost :: IO a
    badRequestWithNoHost = do
        let msg = mconcat [
                    fromByteString "HTTP/1.1 400 Bad Request\r\n\r\n"
                  , fromByteString "400 Bad Request: HTTP/1.1 request with no "
                  , fromByteString "Host header\r\n"
                  , flush
                  ]
        writeEndB <- newBuffer
        Streams.write (Just msg) writeEndB
        Streams.write Nothing writeEndB
        terminateSession BadRequestException

    --------------------------------------------------------------------------
    {-# INLINE checkExpect100Continue #-}
    checkExpect100Continue req =
        when (getHeader "Expect" req == Just "100-continue") $ do
            let (major, minor) = rqVersion req
            let hl = mconcat [ fromByteString "HTTP/"
                             , fromShow major
                             , fromChar '.'
                             , fromShow minor
                             , fromByteString " 100 Continue\r\n\r\n"
                             , flush
                             ]
            os <- newBuffer
            Streams.write (Just hl) os

    --------------------------------------------------------------------------
    {-# INLINE processRequest #-}
    processRequest !hookState !req = do
        -- successfully parsed a request, so restart the timer
        tickle $ max defaultTimeout

        -- check for Expect: 100-continue
        checkExpect100Continue req
        runServerHandler hookState req
            `catches` [ Handler $ escapeSnapHandler hookState
                      , Handler $
                        catchUserException hookState "user handler" req
                      ]

    --------------------------------------------------------------------------
    {-# INLINE runServerHandler #-}
    runServerHandler !hookState !req = do
        (_, rsp0) <- serverHandler config sessionData req
        userHandlerFinishedHook hookState req rsp0

        -- check whether we should close the connection after sending the
        -- response
        let v    = rqVersion req
        let rsp1 = rsp0 { rspHttpVersion = rqVersion req }
        checkConnectionClose v (rspHeaders rsp1)
        cc <- readIORef forceConnectionClose
        let rsp2 = if cc then (setHeader "Connection" "close" rsp1) else rsp1

        -- skip unread portion of request body if rspTransformingRqBody is not
        -- true
        unless (rspTransformingRqBody rsp2) $ Streams.skipToEof (rqBody req)
        date <- getDateString
        let insServer = if isJust (getHeader "Server" rsp2)
                          then id
                          else H.set "Server" sERVER_HEADER
        let !rsp = updateHeaders (insServer . H.set "Date" date) rsp2
        bytesSent <- sendResponse rsp `catch`
                     catchUserException hookState "sending-response" req
        let rspFinal = maybe (setContentLength bytesSent rsp)
                             (const rsp)
                             (rspContentLength rsp)
        dataFinishedHook hookState req rspFinal
        logAccess req rspFinal
        cc' <- readIORef forceConnectionClose
        if cc'
          then return $! ()
          else writeIORef isNewConnection False >> begin

    --------------------------------------------------------------------------
    escapeSnapHandler hookState (EscapeHttp escapeHandler) = do
        escapeHook hookState
        newBuffer >>= escapeHandler tickle readEnd
    escapeSnapHandler _ (TerminateConnection e) = terminateSession e

    --------------------------------------------------------------------------
    catchUserException :: IORef hookState
                       -> ByteString
                       -> Request
                       -> SomeException
                       -> IO a
    catchUserException hookState phase req e = do
        logError $ mconcat [
            fromByteString "Exception leaked to httpSession during phase '"
          , fromByteString phase
          , fromByteString "': \n"
          , requestErrorMessage req e
          ]
        -- Note: the handler passed to httpSession needs to catch its own
        -- exceptions if it wants to avoid an ungracious exit here.
        eatException $ exceptionHook hookState e
        terminateSession e

    --------------------------------------------------------------------------
    sendResponse :: Response -> IO Int64
    sendResponse !rsp0 = do
        let !rsp1 = renderCookies rsp0
        let !code = rspStatus rsp1
        let (rsp, shouldClose) = if isNothing (rspContentLength rsp1) &&
                                    code /= 204 && code /= 304
                                   then noCL rsp1
                                   else (rsp1, False)
        when shouldClose $ writeIORef forceConnectionClose True
        let (!headerBuilder, !hlen) = mkHeaderBuilder rsp
        nBodyBytes <- case rspBody rsp of
                        Stream s ->
                            whenStream headerBuilder hlen rsp s
                        SendFile f Nothing ->
                            whenSendFile headerBuilder rsp f 0
                        -- ignore end length here because we know we had a
                        -- content-length, use that instead.
                        SendFile f (Just (st, _)) ->
                            whenSendFile headerBuilder rsp f st
        return $! nBodyBytes - fromIntegral hlen

    --------------------------------------------------------------------------
    noCL :: Response -> (Response, Bool)
    noCL r =
        if rspHttpVersion r >= (1,1)
          then let r'       = setHeader "Transfer-Encoding" "chunked" r
                   origBody = rspBodyToEnum $ rspBody r
                   body     = \os -> do
                               os' <- writeChunkedTransferEncoding os
                               origBody os'
               in (r' { rspBody = Stream body }, False)
        else
           -- HTTP/1.0 and no content-length? We'll have to close the
           -- socket.
           (setHeader "Connection" "close" r, True)
    {-# INLINE noCL #-}

    --------------------------------------------------------------------------
    -- | If the response contains a content-length, make sure the response body
    -- StreamProc doesn't yield more (or fewer) than the given number of bytes.
    limitRspBody :: Int                      -- ^ header length
                 -> Response                 -- ^ response
                 -> OutputStream ByteString  -- ^ write end of socket
                 -> IO (OutputStream ByteString)
    limitRspBody hlen rsp os = maybe (return os) f $ rspContentLength rsp
      where
        f cl = Streams.giveExactly (fromIntegral hlen + cl) os
    {-# INLINE limitRspBody #-}

    --------------------------------------------------------------------------
    whenStream :: Builder       -- ^ headers
               -> Int           -- ^ header length
               -> Response      -- ^ response
               -> StreamProc    -- ^ output body
               -> IO Int64      -- ^ returns number of bytes written
    whenStream headerString hlen rsp body = do
        -- note:
        --
        --  * precondition here is that we have a content-length and that we're
        --    not using chunked transfer encoding.
        --
        --  * "headerString" includes http status line.
        --
        writeEnd0 <- Streams.ignoreEof writeEnd
        (writeEnd1, getCount) <- Streams.countOutput writeEnd0
        writeEnd2 <- limitRspBody hlen rsp writeEnd1
        writeEndB <- Streams.unsafeBuilderStream (return buffer) writeEnd2
        Streams.write (Just headerString) writeEndB
        writeEnd' <- body writeEndB
        Streams.write Nothing writeEnd'
        -- Just in case the user handler didn't.
        Streams.write Nothing writeEnd1
        n <- getCount
        return $! n - fromIntegral hlen
    {-# INLINE whenStream #-}

    --------------------------------------------------------------------------
    whenSendFile :: Builder     -- ^ headers
                 -> Response    -- ^ response
                 -> FilePath    -- ^ file to serve
                 -> Int64       -- ^ file start offset
                 -> IO Int64    -- ^ returns number of bytes written
    whenSendFile headerString rsp filePath offset = do
        let !cl = fromJust $ rspContentLength rsp
        sendfileHandler buffer headerString filePath offset cl
        return cl
    {-# INLINE whenSendFile #-}


--------------------------------------------------------------------------
mkHeaderBuilder :: Response -> (Builder, Int)
mkHeaderBuilder r = (builder, outlen)
  where
    (major, minor) = rspHttpVersion r
    (hdrs, !hlen)  = buildHdrs $ headers r
    majstr         = show major
    minstr         = show minor
    !majlen        = length majstr
    !minlen        = length minstr
    statstr        = show $ rspStatus r
    !statlen       = length statstr
    crlf           = fromByteString "\r\n"
    space          = fromChar ' '
    reason         = rspStatusReason r
    builder        = mconcat [ fromByteString "HTTP/"
                             , fromString majstr
                             , fromChar '.'
                             , fromString minstr
                             , space
                             , fromString $ statstr
                             , space
                             , fromByteString reason
                             , crlf
                             , hdrs
                             , crlf
                             ]
    !outlen        = 12 + majlen + minlen + statlen + S.length reason + hlen


--------------------------------------------------------------------------
buildHdrs :: Headers -> (Builder,Int)
buildHdrs hdrs =
    H.fold f (mempty, 0) hdrs
  where
    f (!b, !len) !k !y =
        let k'    = CI.original k
            kb    = fromByteString k' <> fromByteString ": "
            !len' = S.length k' + S.length y + 4
            b'    = mconcat [ b
                            , kb
                            , fromByteString y
                            , crlf
                            ]
        in (b', len + len')

    crlf = fromByteString "\r\n"


------------------------------------------------------------------------------
sERVER_HEADER :: ByteString
sERVER_HEADER = S.concat ["Snap/", snapServerVersion]


------------------------------------------------------------------------------
snapServerVersion :: ByteString
snapServerVersion = S.pack $ showVersion $ V.version


------------------------------------------------------------------------------
toHeaders :: [(ByteString, ByteString)] -> H.Headers
toHeaders = H.fromList . map (first CI.mk)


------------------------------------------------------------------------------
terminateSession :: Exception e => e -> IO a
terminateSession = throwIO . TerminateSessionException . SomeException


------------------------------------------------------------------------------
requestErrorMessage :: Request -> SomeException -> Builder
requestErrorMessage req e =
    mconcat [ fromByteString "During processing of request from "
            , fromByteString $ rqClientAddr req
            , fromByteString ":"
            , fromShow $ rqClientPort req
            , fromByteString "\nrequest:\n"
            , fromShow $ show req
            , fromByteString "\n"
            , msgB
            ]
  where
    msgB = mconcat [
             fromByteString "A web handler threw an exception. Details:\n"
           , fromShow e
           ]


------------------------------------------------------------------------------
-- | Convert 'Cookie' into 'ByteString' for output.
cookieToBS :: Cookie -> ByteString
cookieToBS (Cookie k v mbExpTime mbDomain mbPath isSec isHOnly) = cookie
  where
    cookie  = S.concat [k, "=", v, path, exptime, domain, secure, hOnly]
    path    = maybe "" (S.append "; path=") mbPath
    domain  = maybe "" (S.append "; domain=") mbDomain
    exptime = maybe "" (S.append "; expires=" . fmt) mbExpTime
    secure  = if isSec then "; Secure" else ""
    hOnly   = if isHOnly then "; HttpOnly" else ""
    fmt     = S.pack . formatTime defaultTimeLocale
                                  "%a, %d-%b-%Y %H:%M:%S GMT"


--------------------------------------------------------------------------
renderCookies :: Response -> Response
renderCookies r = updateHeaders f r
  where
    f h = if null cookies
            then h
            else foldl' (\m v -> H.insert "Set-Cookie" v m) h cookies
    cookies = fmap cookieToBS . Map.elems $ rspCookies r
