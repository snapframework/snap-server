{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.Session
  ( httpAcceptLoop
  , httpSession
  , snapToServerHandler
  , BadRequestException(..)
  , LengthRequiredException(..)
  , TerminateSessionException(..)
  ) where

------------------------------------------------------------------------------
import           Control.Applicative                      ((<$>))
import           Control.Arrow                            (first, second)
import           Control.Concurrent                       (MVar, ThreadId, killThread, myThreadId, newEmptyMVar, putMVar, readMVar, takeMVar)
import           Control.Concurrent.Extended              (forkIOLabeledWithUnmaskBs, forkOnLabeledWithUnmaskBs)
import           Control.Exception                        (AsyncException, Exception, Handler (..), IOException, SomeException (..))
import qualified Control.Exception                        as E
import           Control.Monad                            (unless, void, when, (>=>))
import           Data.ByteString.Char8                    (ByteString)
import qualified Data.ByteString.Char8                    as S
import qualified Data.ByteString.Unsafe                   as S
import qualified Data.CaseInsensitive                     as CI
import           Data.Int                                 (Int64)
import           Data.IORef                               (IORef, newIORef, readIORef, writeIORef)
import           Data.List                                (foldl')
import qualified Data.Map                                 as Map
import           Data.Maybe                               (fromJust, fromMaybe, isNothing)
import           Data.Monoid                              (mconcat, (<>))
import           Data.Time.Format                         (formatTime)
import           Data.Typeable                            (Typeable)
import           Data.Version                             (showVersion)
import           Data.Word                                (Word64, Word8)
import           Foreign.Marshal.Utils                    (copyBytes)
import           Foreign.Ptr                              (Ptr, castPtr, plusPtr)
import           Foreign.Storable                         (pokeByteOff)
import           System.Locale                            (defaultTimeLocale)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (Builder, flush, fromByteString)
import           Blaze.ByteString.Builder.Char8           (fromChar, fromShow)
import           Blaze.ByteString.Builder.Internal        (Write, defaultBufferSize, exactWrite, fromWrite, getBound)
import           Blaze.ByteString.Builder.Internal.Buffer (Buffer, allocBuffer)
import           System.IO.Streams                        (InputStream, OutputStream)
import qualified System.IO.Streams                        as Streams
------------------------------------------------------------------------------
import qualified Paths_snap_server                        as V
import           Snap.Core                                (EscapeSnap (..))
import           Snap.Core                                (Snap, runSnap)
import           Snap.Internal.Http.Server.Common         (eatException)
import           Snap.Internal.Http.Server.Date           (getCurrentDateTime, getDateString)
import           Snap.Internal.Http.Server.Parser         (IRequest (..), getStdConnection, getStdContentLength, getStdContentType, getStdCookie, getStdHost, getStdTransferEncoding, parseCookie, parseRequest, parseUrlEncoded, readChunkedTransferEncoding, writeChunkedTransferEncoding)
import           Snap.Internal.Http.Server.TimeoutManager (TimeoutManager)
import qualified Snap.Internal.Http.Server.TimeoutManager as TM
import           Snap.Internal.Http.Server.Types          (AcceptFunc (..), PerSessionData (..), SendFileHandler, ServerConfig (..), ServerHandler)
import           Snap.Internal.Http.Types                 (Cookie (..), HttpVersion, Method (..), Request (..), Response (..), ResponseBody (..), StreamProc, getHeader, headers, rspBodyToEnum, updateHeaders)
import           Snap.Internal.Parsing                    (unsafeFromNat)
import           Snap.Internal.Types                      (fixupResponse)
import           Snap.Types.Headers                       (Headers)
import qualified Snap.Types.Headers                       as H


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
snapToServerHandler :: Snap a -> ServerHandler hookState
snapToServerHandler !snap !serverConfig !perSessionData !req =
    runSnap snap logErr tickle req
  where
    logErr = _logError serverConfig . fromByteString
    tickle = _twiddleTimeout perSessionData


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
-- | The main Snap webserver loop. Given a server handler, configuration, and a
-- function to accept new connections, runs an HTTP loop forever over N
-- threads, until a ThreadKilled exception is received.
httpAcceptLoop :: forall hookState .
                  ServerHandler hookState  -- ^ server handler
               -> ServerConfig hookState   -- ^ server config
               -> AcceptFunc               -- ^ accept function
               -> IO ()
httpAcceptLoop serverHandler serverConfig acceptFunc = runLoops
  where
    --------------------------------------------------------------------------
    logError       = _logError serverConfig
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
    runLoops = E.bracket (mapM newLoop [0 .. (nLoops - 1)])
                         (mapM_ killLoop)
                         (mapM_ waitLoop)

    --------------------------------------------------------------------------
    loop :: MVar ()
         -> TimeoutManager
         -> (forall a. IO a -> IO a)
         -> IO ()
    loop mv tm loopRestore = eatException go `E.finally` putMVar mv ()
      where
        ----------------------------------------------------------------------
        handlers =
            [ Handler $ \(e :: IOException)    -> logException e >> go
            , Handler $ \(e :: AsyncException) -> loopRestore (E.throwIO $! e)
            , Handler $ \(e :: SomeException)  -> do logException e
                                                     loopRestore (E.throwIO e)
            ]

        go = do
            (sendFileHandler, localAddress, localPort, remoteAddress,
             remotePort, readEnd, writeEnd,
             cleanup) <- runAcceptFunc acceptFunc loopRestore
                                       `E.catches` handlers
            let threadLabel = S.concat [ "snap-server: client "
                                       , remoteAddress
                                       , ":"
                                       , S.pack $ show remotePort
                                       ]
            forkIOLabeledWithUnmaskBs threadLabel
                $ \restore ->
                   eatException $
                   prep sendFileHandler localAddress localPort remoteAddress
                        remotePort readEnd writeEnd cleanup restore
            go

        prep :: SendFileHandler
             -> ByteString
             -> Int
             -> ByteString
             -> Int
             -> InputStream ByteString
             -> OutputStream ByteString
             -> IO ()
             -> (forall a . IO a -> IO a)
             -> IO ()
        prep sendFileHandler localAddress localPort remoteAddress remotePort
             readEnd writeEnd cleanup restore =
          do
            connClose <- newIORef False
            newConn   <- newIORef True
            tid       <- myThreadId
            tmHandle  <- TM.register (killThread tid) tm
            let twiddleTimeout = TM.modify tmHandle

            let !psd = PerSessionData connClose
                                      twiddleTimeout
                                      newConn
                                      sendFileHandler
                                      localAddress
                                      localPort
                                      remoteAddress
                                      remotePort
                                      readEnd
                                      writeEnd
            restore (session psd) `E.finally` cleanup

    --------------------------------------------------------------------------
    session psd = do
        buffer <- allocBuffer defaultBufferSize
        httpSession buffer serverHandler serverConfig psd

    --------------------------------------------------------------------------
    newLoop cpu = E.mask_ $ do
        mv  <- newEmptyMVar
        tm  <- TM.initialize defaultTimeout getCurrentDateTime
        let threadLabel = S.concat [ "snap-server: accept loop #"
                                   , S.pack $ show cpu
                                   ]

        tid <- forkOnLabeledWithUnmaskBs threadLabel cpu $ loop mv tm
        return $! EventLoopCpu tid tm mv

    --------------------------------------------------------------------------
    waitLoop (EventLoopCpu _ _ mv) = readMVar mv

    --------------------------------------------------------------------------
    killLoop ev = E.mask_ $ do
        killThread tid
        TM.stop tm
        takeMVar mv >>= E.evaluate
      where
        tid = _acceptThread ev
        tm  = _timeoutManager ev
        mv  = _exitMVar ev

------------------------------------------------------------------------------
httpSession :: forall hookState .
               Buffer
            -> ServerHandler hookState
            -> ServerConfig hookState
            -> PerSessionData
            -> IO ()
httpSession !buffer !serverHandler !config !sessionData = loop
  where
    --------------------------------------------------------------------------
    defaultTimeout          = _defaultTimeout config
    isSecure                = _isSecure config
    localHostname           = _localHostname config
    logAccess               = _logAccess config
    logError                = _logError config
    newRequestHook          = _onNewRequest config
    parseHook               = _onParse config
    userHandlerFinishedHook = _onUserHandlerFinished config
    dataFinishedHook        = _onDataFinished config
    exceptionHook           = _onException config
    escapeHook              = _onEscape config

    --------------------------------------------------------------------------
    forceConnectionClose    = _forceConnectionClose sessionData
    isNewConnection         = _isNewConnection sessionData
    localAddress            = _localAddress sessionData
    localPort               = _localPort sessionData
    remoteAddress           = _remoteAddress sessionData
    remotePort              = _remotePort sessionData
    readEnd                 = _readEnd sessionData
    tickle f                = _twiddleTimeout sessionData f
    writeEnd                = _writeEnd sessionData
    sendfileHandler         = _sendfileHandler sessionData

    --------------------------------------------------------------------------
    newBuffer :: IO (OutputStream Builder)
    newBuffer = Streams.unsafeBuilderStream (return buffer) writeEnd

    --------------------------------------------------------------------------
    -- Begin HTTP session processing.
    loop :: IO ()
    loop = do
        -- peek first to ensure startHook gets generated at the right time.
        readEndAtEof >>= (flip unless $ do
            hookState <- newRequestHook sessionData >>= newIORef
            -- parse HTTP request
            req <- receiveRequest
            parseHook hookState req
            processRequest hookState req)

    ------------------------------------------------------------------------------
    readEndAtEof = Streams.read readEnd >>=
                   maybe (return True)
                         (\c -> if S.null c
                                  then readEndAtEof
                                  else Streams.unRead c readEnd >> return False)
    {-# INLINE readEndAtEof #-}

    --------------------------------------------------------------------------
    -- Read the HTTP request from the socket, parse it, and pre-process it.
    receiveRequest :: IO Request
    receiveRequest = {-# SCC "httpSession/receiveRequest" #-} do
        readEnd' <- Streams.throwIfProducesMoreThan mAX_HEADERS_SIZE readEnd
        parseRequest readEnd' >>= toRequest
    {-# INLINE receiveRequest #-}

    --------------------------------------------------------------------------
    toRequest :: IRequest -> IO Request
    toRequest !ireq = {-# SCC "httpSession/toRequest" #-} do
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
        !readEnd' <- setupReadEnd

        -- Parse an application/x-www-form-urlencoded form, if it was sent
        (!readEnd'', postParams) <- parseForm readEnd'

        let allParams = Map.unionWith (++) queryParams postParams

        -- Decide whether the connection should be closed after the response is
        -- sent (stored in the forceConnectionClose IORef).
        checkConnectionClose version $ getStdConnection stdHdrs

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
        !method       = iMethod ireq
        !version      = iHttpVersion ireq
        !stdHdrs      = iStdHeaders ireq
        !hdrs         = iRequestHeaders ireq

        !isHttp11     = version >= (1, 1)

        !mbHost       = getStdHost stdHdrs
        !localHost    = fromMaybe localHostname mbHost
        mbCL          = unsafeFromNat <$>
                        getStdContentLength stdHdrs
        !isChunked    = (CI.mk <$> getStdTransferEncoding stdHdrs)
                            == Just "chunked"
        cookies       = fromMaybe [] (getStdCookie stdHdrs >>= parseCookie)
        contextPath   = "/"
        !uri          = iRequestUri ireq
        queryParams   = parseUrlEncoded queryString
        emptyParams   = Map.empty

        ----------------------------------------------------------------------
        (pathInfo, queryString) = first dropLeadingSlash . second (S.drop 1)
                                    $ S.break (== '?') uri

        ----------------------------------------------------------------------
        dropLeadingSlash s = if S.null s
                               then s
                               else let !a = S.unsafeIndex s 0
                                    in if a == 47   -- 47 == '/'
                                         then S.unsafeDrop 1 s
                                         else s
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
              else maybe (const noContentLength)
                         (Streams.takeBytes . fromIntegral) mbCL readEnd
        {-# INLINE setupReadEnd #-}

        ----------------------------------------------------------------------
        -- | If a request is not in chunked transfer encoding and lacks a
        -- content-length, the request body is null string.
        noContentLength :: IO (InputStream ByteString)
        noContentLength = do
            when (method == POST || method == PUT) return411
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
            mbCT    = trimIt <$> getStdContentType stdHdrs
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
    checkConnectionClose version connection = do
        -- For HTTP/1.1: if there is an explicit Connection: close, we'll close
        -- the socket later.
        --
        -- For HTTP/1.0: if there is no explicit Connection: Keep-Alive,
        -- close the socket later.
        let v = CI.mk <$> connection
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
        when (getHeader "expect" req == Just "100-continue") $ do
            let v = if rqVersion req == (1,1) then "HTTP/1.1" else "HTTP/1.0"

            let hl = fromByteString v                       <>
                     fromByteString " 100 Continue\r\n\r\n" <>
                     flush
            os <- newBuffer
            Streams.write (Just hl) os

    --------------------------------------------------------------------------
    {-# INLINE processRequest #-}
    processRequest !hookState !req = {-# SCC "httpSession/processRequest" #-} do
        -- successfully parsed a request, so restart the timer
        tickle $ max defaultTimeout

        -- check for Expect: 100-continue
        checkExpect100Continue req
        b <- runServerHandler hookState req
               `E.catches` [ Handler $ escapeSnapHandler hookState
                           , Handler $
                             catchUserException hookState "user handler" req
                           ]
        if b
          then do writeIORef isNewConnection False
                  -- the timer resets to its default value here.
                  loop
          else return $! ()

    --------------------------------------------------------------------------
    {-# INLINE runServerHandler #-}
    runServerHandler !hookState !req = {-# SCC "httpSession/runServerHandler" #-} do
        (_, rsp0) <- serverHandler config sessionData req
        userHandlerFinishedHook hookState req rsp0

        -- check whether we should close the connection after sending the
        -- response
        let v      = rqVersion req
        let is_1_0 = (v == (1,0))
        cc <- if is_1_0 && (isNothing $ rspContentLength rsp0)
                then return $! True
                else readIORef forceConnectionClose

        -- skip unread portion of request body if rspTransformingRqBody is not
        -- true
        unless (rspTransformingRqBody rsp0) $ Streams.skipToEof (rqBody req)

        !date <- getDateString
        let (!hdrs, !cc') = addDateAndServerHeaders is_1_0 date cc $
                            headers rsp0
        let rsp = updateHeaders (const hdrs) rsp0
        writeIORef forceConnectionClose cc'
        bytesSent <- sendResponse req rsp `E.catch`
                     catchUserException hookState "sending-response" req
        dataFinishedHook hookState req rsp
        logAccess req rsp bytesSent
        return $! not cc'

    --------------------------------------------------------------------------
    addDateAndServerHeaders !is1_0 !date !cc !hdrs =
        {-# SCC "addDateAndServerHeaders" #-}
        let (!hdrs', !newcc) = go [("date",date)] False cc
                                 $ H.unsafeToCaseFoldedList hdrs
        in (H.unsafeFromCaseFoldedList hdrs', newcc)
      where
        -- N.B.: here we know the date header has already been removed by
        -- "fixupResponse".
        go !l !seenServer !connClose [] =
            let !l1 = if seenServer then l else (("server", sERVER_HEADER):l)
                !l2 = if connClose then (("connection", "close"):l1) else l1
            in (l2, connClose)
        go l _ c (x@("server",_):xs) = go (x:l) True c xs
        go l seenServer c (x@("connection", v):xs)
              | c = go l seenServer c xs
              | v == "close" || (is1_0 && v /= "keep-alive") =
                     go l seenServer True xs
              | otherwise = go (x:l) seenServer c xs
        go l seenServer c (x:xs) = go (x:l) seenServer c xs

    --------------------------------------------------------------------------
    escapeSnapHandler hookState (EscapeHttp escapeHandler) = do
        escapeHook hookState
        newBuffer >>= escapeHandler tickle readEnd
        return False
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
    sendResponse :: Request -> Response -> IO Word64
    sendResponse !req !rsp0 = {-# SCC "httpSession/sendResponse" #-} do
        let !v          = rqVersion req
        rsp            <- fixupResponse req rsp0
        let !hdrs'      = renderCookies rsp (headers rsp)
        let !code       = rspStatus rsp
        let body        = rspBody rsp
        let needChunked = rqMethod req /= HEAD
                            && isNothing (rspContentLength rsp)
                            && code /= 204
                            && code /= 304

        let (hdrs'', body', shouldClose) = if needChunked
                                             then noCL req hdrs' body
                                             else (hdrs', body, False)

        when shouldClose $ writeIORef forceConnectionClose $! True
        let hdrWrite      = mkHeaderWrite v rsp hdrs''
        let hlen          = getBound hdrWrite
        let headerBuilder = fromWrite hdrWrite

        nBodyBytes <- case body' of
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
    noCL :: Request
         -> Headers
         -> ResponseBody
         -> (Headers, ResponseBody, Bool)
    noCL req hdrs body =
        if v == (1,1)
          then let origBody = rspBodyToEnum body
                   body'    = \os -> do
                                 os' <- writeChunkedTransferEncoding os
                                 origBody os'
               in ( H.set "transfer-encoding" "chunked" hdrs
                  , Stream body'
                  , False)
          else
            -- We've already noted that we have to close the socket earlier in
            -- runServerHandler.
            (hdrs, body, True)
      where
        v = rqVersion req
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
        f cl = Streams.giveExactly (fromIntegral hlen + fromIntegral cl) os
    {-# INLINE limitRspBody #-}

    --------------------------------------------------------------------------
    whenStream :: Builder       -- ^ headers
               -> Int           -- ^ header length
               -> Response      -- ^ response
               -> StreamProc    -- ^ output body
               -> IO Word64      -- ^ returns number of bytes written
    whenStream headerString hlen rsp body = do
        -- note:
        --
        --  * precondition here is that we have a content-length and that we're
        --    not using chunked transfer encoding.
        --
        --  * "headerString" includes http status line.
        --
        -- If you're transforming the request body, you have to manage your own
        -- timeouts.
        let t = if rspTransformingRqBody rsp
                  then return $! ()
                  else tickle $ max defaultTimeout
        writeEnd0 <- Streams.ignoreEof writeEnd
        (writeEnd1, getCount) <- Streams.countOutput writeEnd0
        writeEnd2 <- limitRspBody hlen rsp writeEnd1
        writeEndB <- Streams.unsafeBuilderStream (return buffer) writeEnd2 >>=
                     Streams.contramapM (\x -> t >> return x)

        Streams.write (Just headerString) writeEndB
        writeEnd' <- body writeEndB
        Streams.write Nothing writeEnd'
        -- Just in case the user handler didn't.
        Streams.write Nothing writeEnd1
        n <- getCount
        return $! fromIntegral n - fromIntegral hlen
    {-# INLINE whenStream #-}

    --------------------------------------------------------------------------
    whenSendFile :: Builder     -- ^ headers
                 -> Response    -- ^ response
                 -> FilePath    -- ^ file to serve
                 -> Word64      -- ^ file start offset
                 -> IO Word64   -- ^ returns number of bytes written
    whenSendFile headerString rsp filePath offset = do
        let !cl = fromJust $ rspContentLength rsp
        sendfileHandler buffer headerString filePath offset cl
        return cl
    {-# INLINE whenSendFile #-}


--------------------------------------------------------------------------
mkHeaderLine :: HttpVersion -> Response -> Write
mkHeaderLine outVer r =
    case outCode of
        200 | outVer == (1, 1) ->
                  exactWrite 17 (void . cpBS "HTTP/1.1 200 OK\r\n")
        200 | otherwise ->
                  exactWrite 17 (void . cpBS "HTTP/1.0 200 OK\r\n")
        _ -> exactWrite len (void . line)
  where
    outCode = rspStatus r

    v = if outVer == (1,1) then "HTTP/1.1 " else "HTTP/1.0 "

    outCodeStr = S.pack $ show outCode
    space !op = do
        pokeByteOff op 0 (32 :: Word8)
        return $! plusPtr op 1

    line = cpBS v >=> cpBS outCodeStr >=> space >=> cpBS reason
                  >=> crlfPoke

    reason = rspStatusReason r
    len = 12 + S.length outCodeStr + S.length reason


--------------------------------------------------------------------------
mkHeaderWrite :: HttpVersion -> Response -> Headers -> Write
mkHeaderWrite v r hdrs = mkHeaderLine v r <> headersToWrite hdrs


------------------------------------------------------------------------------
{-# INLINE headersToWrite #-}
headersToWrite :: Headers -> Write
headersToWrite hdrs = exactWrite len copy
  where
    len = H.foldedFoldl' f 0 hdrs + 2
      where
        f l k v = l + S.length k + S.length v + 4

    copy = go $ H.unsafeToCaseFoldedList hdrs

    go []         !op = void $ crlfPoke op
    go ((k,v):xs) !op = do
        !op'  <- cpBS k op
        pokeByteOff op' 0 (58 :: Word8)  -- colon
        pokeByteOff op' 1 (32 :: Word8)  -- space
        !op''  <- cpBS v $ plusPtr op' 2
        crlfPoke op'' >>= go xs


{-# INLINE cpBS #-}
cpBS :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
cpBS s !op = S.unsafeUseAsCStringLen s $ \(cstr, clen) -> do
                let !cl = fromIntegral clen
                copyBytes op (castPtr cstr) cl
                return $! plusPtr op cl

{-# INLINE crlfPoke #-}
crlfPoke :: Ptr Word8 -> IO (Ptr Word8)
crlfPoke !op = do
    pokeByteOff op 0 (13 :: Word8)  -- cr
    pokeByteOff op 1 (10 :: Word8)  -- lf
    return $! plusPtr op 2


------------------------------------------------------------------------------
sERVER_HEADER :: ByteString
sERVER_HEADER = S.concat ["Snap/", snapServerVersion]


------------------------------------------------------------------------------
snapServerVersion :: ByteString
snapServerVersion = S.pack $ showVersion $ V.version


------------------------------------------------------------------------------
terminateSession :: Exception e => e -> IO a
terminateSession = E.throwIO . TerminateSessionException . SomeException


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


------------------------------------------------------------------------------
renderCookies :: Response -> Headers -> Headers
renderCookies r hdrs
    | null cookies = hdrs
    | otherwise = foldl' (\m v -> H.unsafeInsert "set-cookie" v m) hdrs cookies

  where
    cookies = fmap cookieToBS . Map.elems $ rspCookies r
