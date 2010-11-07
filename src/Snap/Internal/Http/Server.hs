{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server where

------------------------------------------------------------------------------
import           Control.Arrow (first, second)
import           Control.Monad.State.Strict
import           Control.Exception
import           Data.Char
import           Data.CIByteString
import           Data.Binary.Put
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Nums.Careless.Int as Cvt
import           Data.Int
import           Data.IORef
import           Data.Iteratee.WrappedByteString (unWrap)
import           Data.List (foldl')
import qualified Data.Map as Map
import           Data.Maybe (fromJust, catMaybes, fromMaybe)
import           Data.Monoid
import           Data.Version
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           GHC.Conc
import           Prelude hiding (catch, show, Show)
import qualified Prelude
import           System.PosixCompat.Files hiding (setFileSize)
import           System.Posix.Types (FileOffset)
import           Text.Show.ByteString hiding (runPut)
------------------------------------------------------------------------------
import           System.FastLogger
import           Snap.Internal.Debug
import           Snap.Internal.Http.Types hiding (Enumerator)
import           Snap.Internal.Http.Parser
import           Snap.Internal.Http.Server.Date

import           Snap.Internal.Http.Server.Backend
import           Snap.Internal.Http.Server.HttpPort
import qualified Snap.Internal.Http.Server.GnuTLS as TLS
import           Snap.Internal.Http.Server.SimpleBackend

import           Snap.Internal.Iteratee.Debug
import           Snap.Iteratee hiding (foldl', head, take, FileOffset)
import qualified Snap.Iteratee as I

import qualified Paths_snap_server as V


------------------------------------------------------------------------------
-- | The handler has to return the request object because we have to clear the
-- HTTP request body before we send the response. If the handler consumes the
-- request body, it is responsible for setting @rqBody=return@ in the returned
-- request (otherwise we will mess up reading the input stream).
--
-- Note that we won't be bothering end users with this -- the details will be
-- hidden inside the Snap monad
type ServerHandler = (ByteString -> IO ())
                   -> Request
                   -> Iteratee IO (Request,Response)

type ServerMonad = StateT ServerState (Iteratee IO)

data ListenPort = HttpPort  ByteString Int                   -- (bind address, port)
                | HttpsPort ByteString Int FilePath FilePath -- (bind address, port, path to certificate, path to key)

data EventLoopType = EventLoopSimple
                   | EventLoopLibEv
  deriving (Prelude.Show)

defaultEvType :: EventLoopType
defaultEvType = EventLoopSimple

data ServerState = ServerState
    { _forceConnectionClose  :: Bool
    , _localHostname         :: ByteString
    , _sessionPort           :: SessionInfo
    , _logAccess             :: Request -> Response -> IO ()
    , _logError              :: ByteString -> IO ()
    }

------------------------------------------------------------------------------
runServerMonad :: ByteString                     -- ^ local host name
               -> SessionInfo                    -- ^ session port information
               -> (Request -> Response -> IO ()) -- ^ access log function
               -> (ByteString -> IO ())          -- ^ error log function
               -> ServerMonad a                  -- ^ monadic action to run
               -> Iteratee IO a
runServerMonad lh s la le m = evalStateT m st
  where
    st = ServerState False lh s la le


------------------------------------------------------------------------------
-- input/output


------------------------------------------------------------------------------
httpServe :: [ListenPort]        -- ^ ports to listen on
          -> Maybe EventLoopType -- ^ Specify a given event loop, otherwise a default is picked
          -> ByteString          -- ^ local hostname (server name)
          -> Maybe FilePath      -- ^ path to the access log
          -> Maybe FilePath      -- ^ path to the error log
          -> ServerHandler       -- ^ handler procedure
          -> IO ()
httpServe ports mevType localHostname alogPath elogPath handler =
    withLoggers alogPath elogPath
                (\(alog, elog) -> spawnAll alog elog)

  where
    --------------------------------------------------------------------------
    spawnAll alog elog = {-# SCC "httpServe/spawnAll" #-} do

        let evType = maybe defaultEvType id mevType

        logE elog $ S.concat [ "Server.httpServe: START ("
                             , toBS $ Prelude.show evType, ")"]

        let initHttps = foldr (\p b -> b || case p of { (HttpsPort _ _ _ _) -> True; _ -> False;}) False ports

        if initHttps
            then do TLS.initTLS
                    TLS.setLogging 1 $ \_ s -> logE elog $ toBS s
            else return ()
            
        nports <- mapM bindPort ports
            
        (runEventLoop evType nports numCapabilities (logE elog) $
                      runHTTP alog elog handler localHostname) `finally` do

            logE elog "Server.httpServe: SHUTDOWN"

            if initHttps
                then TLS.stopTLS
                else return ()

            logE elog "Server.httpServe: BACKEND STOPPED"

    --------------------------------------------------------------------------
    bindPort (HttpPort  baddr port)          = bindHttp  baddr port
    bindPort (HttpsPort baddr port cert key) = TLS.bindHttps baddr port cert key


    --------------------------------------------------------------------------
    runEventLoop EventLoopSimple       = simpleEventLoop
    runEventLoop e = \_ _ _ _ -> throwIO $ PatternMatchFail $ Prelude.show e ++ " is not a supported backend"


    --------------------------------------------------------------------------
    maybeSpawnLogger = maybe (return Nothing) $ (liftM Just) . newLogger


    --------------------------------------------------------------------------
    withLoggers afp efp =
        bracket (do alog <- maybeSpawnLogger afp
                    elog <- maybeSpawnLogger efp
                    return (alog, elog))
                (\(alog, elog) -> do
                    maybe (return ()) stopLogger alog
                    maybe (return ()) stopLogger elog)


------------------------------------------------------------------------------
debugE :: (MonadIO m) => ByteString -> m ()
debugE s = debug $ "Server: " ++ (map w2c $ S.unpack s)


------------------------------------------------------------------------------
logE :: Maybe Logger -> ByteString -> IO ()
logE elog = maybe debugE (\l s -> debugE s >> logE' l s) elog


------------------------------------------------------------------------------
logE' :: Logger -> ByteString -> IO ()
logE' logger s = (timestampedLogEntry s) >>= logMsg logger


------------------------------------------------------------------------------
bshow :: (Prelude.Show a) => a -> ByteString
bshow = toBS . Prelude.show


------------------------------------------------------------------------------
logA ::Maybe Logger -> Request -> Response -> IO ()
logA alog = maybe (\_ _ -> return ()) logA' alog


------------------------------------------------------------------------------
logA' :: Logger -> Request -> Response -> IO ()
logA' logger req rsp = do
    let hdrs      = rqHeaders req
    let host      = rqRemoteAddr req
    let user      = Nothing -- TODO we don't do authentication yet
    let (v, v')   = rqVersion req
    let ver       = S.concat [ "HTTP/", bshow v, ".", bshow v' ]
    let method    = toBS $ Prelude.show (rqMethod req)
    let reql      = S.intercalate " " [ method, rqURI req, ver ]
    let status    = rspStatus rsp
    let cl        = rspContentLength rsp
    let referer   = maybe Nothing (Just . head) $ Map.lookup "referer" hdrs
    let userAgent = maybe "-" head $ Map.lookup "user-agent" hdrs

    msg <- combinedLogEntry host user reql status cl referer userAgent
    logMsg logger msg


------------------------------------------------------------------------------
runHTTP :: Maybe Logger                  -- ^ access logger
        -> Maybe Logger                  -- ^ error logger
        -> ServerHandler                 -- ^ handler procedure
        -> ByteString                    -- ^ local host name
        -> SessionInfo                   -- ^ session port information
        -> Enumerator IO ()              -- ^ read end of socket
        -> Iteratee IO ()                -- ^ write end of socket
        -> (FilePath -> Int64 -> Int64 -> IO ())
                                         -- ^ sendfile end
        -> IO ()                         -- ^ timeout tickler
        -> IO ()
runHTTP alog elog handler lh sinfo readEnd writeEnd onSendFile tickle =
    go `catches` [ Handler $ \(e :: AsyncException) -> do
                       throwIO e

                 , Handler $ \(e :: SomeException) ->
                       logE elog $ S.concat [ logPrefix , bshow e ] ]

  where
    logPrefix = S.concat [ "[", remoteAddress sinfo, "]: error: " ]

    go = do
        buf <- mkIterateeBuffer
        let iter1 = runServerMonad lh sinfo (logA alog) (logE elog) $
                                   httpSession writeEnd buf onSendFile tickle
                                   handler
        let iter = iterateeDebugWrapper "httpSession iteratee" iter1
        readEnd iter >>= run
        debug "runHTTP/go: finished"


------------------------------------------------------------------------------
sERVER_HEADER :: [ByteString]
sERVER_HEADER = [S.concat ["Snap/", snapServerVersion]]


------------------------------------------------------------------------------
snapServerVersion :: ByteString
snapServerVersion = SC.pack $ showVersion $ V.version


------------------------------------------------------------------------------
logAccess :: Request -> Response -> ServerMonad ()
logAccess req rsp = gets _logAccess >>= (\l -> liftIO $ l req rsp)


------------------------------------------------------------------------------
logError :: ByteString -> ServerMonad ()
logError s = gets _logError >>= (\l -> liftIO $ l s)


------------------------------------------------------------------------------
-- | Runs an HTTP session.
httpSession :: Iteratee IO ()                -- ^ write end of socket
            -> ForeignPtr CChar              -- ^ iteratee buffer
            -> (FilePath -> Int64 -> Int64 -> IO ())
                                             -- ^ sendfile continuation
            -> IO ()                         -- ^ timeout tickler
            -> ServerHandler                 -- ^ handler procedure
            -> ServerMonad ()
httpSession writeEnd' ibuf onSendFile tickle handler = do

    writeEnd1 <- liftIO $ I.unsafeBufferIterateeWithBuffer ibuf writeEnd'

    let writeEnd = iterateeDebugWrapper "writeEnd" writeEnd1

    liftIO $ debug "Server.httpSession: entered"
    mreq  <- receiveRequest

    -- successfully got a request, so restart timer
    liftIO tickle

    case mreq of
      (Just req) -> do
          liftIO $ debug $ "Server.httpSession: got request: " ++
                           Prelude.show (rqMethod req) ++
                           " " ++ SC.unpack (rqURI req) ++
                           " " ++ Prelude.show (rqVersion req)

          -- check for Expect: 100-continue
          checkExpect100Continue req writeEnd

          logerr <- gets _logError

          (req',rspOrig) <- lift $ handler logerr req

          liftIO $ debug $ "Server.httpSession: finished running user handler"

          let rspTmp = rspOrig { rspHttpVersion = rqVersion req }
          checkConnectionClose (rspHttpVersion rspTmp) (rspHeaders rspTmp)

          cc <- gets _forceConnectionClose
          let rsp = if cc
                      then (setHeader "Connection" "close" rspTmp)
                      else rspTmp

          liftIO $ debug "Server.httpSession: handled, skipping request body"

          srqEnum <- liftIO $ readIORef $ rqBody req'
          let (SomeEnumerator rqEnum) = srqEnum
          lift $ joinIM
               $ rqEnum (iterateeDebugWrapper "httpSession/skipToEof" skipToEof)
          liftIO $ debug $ "Server.httpSession: request body skipped, " ++
                           "sending response"

          date <- liftIO getDateString
          let ins = Map.insert "Date" [date] . Map.insert "Server" sERVER_HEADER
          let rsp' = updateHeaders ins rsp
          (bytesSent,_) <- sendResponse req rsp' writeEnd onSendFile

          liftIO . debug $ "Server.httpSession: sent " ++
                           (Prelude.show bytesSent) ++ " bytes"

          maybe (logAccess req rsp')
                (\_ -> logAccess req $ setContentLength bytesSent rsp')
                (rspContentLength rsp')

          if cc
             then do
                 debug $ "httpSession: Connection: Close, harikari"
                 liftIO $ myThreadId >>= killThread
             else httpSession writeEnd' ibuf onSendFile tickle handler

      Nothing -> do
          liftIO $ debug $ "Server.httpSession: parser did not produce a " ++
                           "request, ending session"
          return ()


------------------------------------------------------------------------------
checkExpect100Continue :: Request
                       -> Iteratee IO ()
                       -> ServerMonad ()
checkExpect100Continue req writeEnd = do
    let mbEx = getHeaders "Expect" req

    maybe (return ())
          (\l -> if elem "100-continue" l then go else return ())
          mbEx

  where
    go = do
        let (major,minor) = rqVersion req
        let hl = runPut $ do
                     putByteString "HTTP/"
                     showp major
                     putAscii '.'
                     showp minor
                     putByteString " 100 Continue\r\n\r\n"
        iter <- liftIO $ enumLBS hl writeEnd
        liftIO $ run iter


------------------------------------------------------------------------------
receiveRequest :: ServerMonad (Maybe Request)
receiveRequest = do
    mreq <- {-# SCC "receiveRequest/parseRequest" #-} lift parseRequest

    case mreq of
      (Just ireq) -> do
          req' <- toRequest ireq
          setEnumerator req'
          req  <- parseForm req'
          checkConnectionClose (rqVersion req) (rqHeaders req)
          return $ Just req

      Nothing     -> return Nothing


  where
    --------------------------------------------------------------------------
    -- check: did the client specify "transfer-encoding: chunked"? then we have
    -- to honor that.
    --
    -- otherwise: check content-length header. if set: only take N bytes from
    -- the read end of the socket
    --
    -- if no content-length and no chunked encoding, enumerate the entire
    -- socket and close afterwards
    setEnumerator :: Request -> ServerMonad ()
    setEnumerator req =
        {-# SCC "receiveRequest/setEnumerator" #-}
        if isChunked
          then do
              liftIO $ debug $ "receiveRequest/setEnumerator: " ++
                               "input in chunked encoding"
              let e = readChunkedTransferEncoding
              liftIO $ writeIORef (rqBody req)
                                  (SomeEnumerator e)
          else maybe noContentLength hasContentLength mbCL

      where
        isChunked = maybe False
                          ((== ["chunked"]) . map toCI)
                          (Map.lookup "transfer-encoding" hdrs)

        hasContentLength :: Int -> ServerMonad ()
        hasContentLength l = do
            liftIO $ debug $ "receiveRequest/setEnumerator: " ++
                             "request had content-length " ++ Prelude.show l
            liftIO $ writeIORef (rqBody req) (SomeEnumerator e)
            liftIO $ debug "receiveRequest/setEnumerator: body enumerator set"
          where
            e :: Enumerator IO a
            e it = return $ joinI $ I.take l $
                iterateeDebugWrapper "rqBody iterator" it

        noContentLength :: ServerMonad ()
        noContentLength = do
            liftIO $ debug ("receiveRequest/setEnumerator: " ++
                            "request did NOT have content-length")

            -- FIXME: should we not just read everything?
            let e = return . joinI . I.take 0

            liftIO $ writeIORef (rqBody req) (SomeEnumerator e)
            liftIO $ debug "receiveRequest/setEnumerator: body enumerator set"


        hdrs = rqHeaders req
        mbCL = Map.lookup "content-length" hdrs >>= return . Cvt.int . head


    --------------------------------------------------------------------------
    parseForm :: Request -> ServerMonad Request
    parseForm req = {-# SCC "receiveRequest/parseForm" #-}
        if doIt then getIt else return req
      where
        mbCT   = liftM head $ Map.lookup "content-type" (rqHeaders req)
        trimIt = fst . SC.spanEnd isSpace . SC.takeWhile (/= ';')
                     . SC.dropWhile isSpace
        mbCT'  = liftM trimIt mbCT
        doIt   = mbCT' == Just "application/x-www-form-urlencoded"

        maximumPOSTBodySize :: Int64
        maximumPOSTBodySize = 10*1024*1024

        getIt :: ServerMonad Request
        getIt = {-# SCC "receiveRequest/parseForm/getIt" #-} do
            liftIO $ debug "parseForm: got application/x-www-form-urlencoded"
            liftIO $ debug "parseForm: reading POST body"
            senum <- liftIO $ readIORef $ rqBody req
            let (SomeEnumerator enum) = senum
            let i = joinI $ takeNoMoreThan maximumPOSTBodySize stream2stream
            iter <- liftIO $ enum i
            body <- liftM unWrap $ lift iter
            let newParams = parseUrlEncoded body

            liftIO $ debug "parseForm: stuffing 'enumBS body' into request"

            let e = enumBS body >. enumEof

            liftIO $ writeIORef (rqBody req) $ SomeEnumerator $
                     e . iterateeDebugWrapper "regurgitate body"

            return $ req { rqParams = rqParams req `mappend` newParams }


    --------------------------------------------------------------------------
    toRequest (IRequest method uri version kvps) =
        {-# SCC "receiveRequest/toRequest" #-} do
            localAddr     <- gets $ localAddress . _sessionPort
            lport         <- gets $ localPort . _sessionPort
            remoteAddr    <- gets $ remoteAddress . _sessionPort
            rport         <- gets $ remotePort . _sessionPort
            localHostname <- gets $ _localHostname
            secure        <- gets $ isSecure . _sessionPort

            let (serverName, serverPort) = fromMaybe
                                             (localHostname, lport)
                                             (liftM (parseHost . head)
                                                    (Map.lookup "host" hdrs))

            -- will override in "setEnumerator"
            enum <- liftIO $ newIORef $ SomeEnumerator return


            return $ Request serverName
                             serverPort
                             remoteAddr
                             rport
                             localAddr
                             lport
                             localHostname
                             secure
                             hdrs
                             enum
                             mbContentLength
                             method
                             version
                             cookies
                             snapletPath
                             pathInfo
                             contextPath
                             uri
                             queryString
                             params

      where
        snapletPath = ""        -- TODO: snaplets in v0.2

        dropLeadingSlash s = maybe s f mbS
          where
            f (a,s') = if a == c2w '/' then s' else s
            mbS = S.uncons s

        hdrs            = toHeaders kvps

        mbContentLength = liftM (Cvt.int . head) $
                          Map.lookup "content-length" hdrs

        cookies         = concat $
                          maybe []
                                (catMaybes . map parseCookie)
                                (Map.lookup "cookie" hdrs)

        contextPath     = "/"

        parseHost h = (a, Cvt.int (S.drop 1 b))
          where
            (a,b) = S.break (== (c2w ':')) h

        params          = parseUrlEncoded queryString

        (pathInfo, queryString) = first dropLeadingSlash . second (S.drop 1) $
                                  S.break (== (c2w '?')) uri


------------------------------------------------------------------------------
-- Response must be well-formed here
sendResponse :: forall a . Request
             -> Response
             -> Iteratee IO a
             -> (FilePath -> Int64 -> Int64 -> IO a)
             -> ServerMonad (Int64, a)
sendResponse req rsp' writeEnd onSendFile = do
    rsp <- fixupResponse rsp'
    let !headerString = mkHeaderString rsp

    (!x,!bs) <- case (rspBody rsp) of
                  (Enum e)             -> lift $ whenEnum headerString rsp e
                  (SendFile f Nothing) -> lift $
                                          whenSendFile headerString rsp f 0
                  (SendFile f (Just (st,_))) ->
                      lift $ whenSendFile headerString rsp f st

    return $! (bs,x)

  where
    --------------------------------------------------------------------------
    whenEnum :: ByteString
             -> Response
             -> (forall x . Enumerator IO x)
             -> Iteratee IO (a,Int64)
    whenEnum hs rsp e = do
        let enum = if rspTransformingRqBody rsp
                     then enumBS hs >. e
                     else enumBS hs >. e >. enumEof

        let hl = fromIntegral $ S.length hs

        debug $ "sendResponse: whenEnum: enumerating bytes"
        (x,bs) <- joinIM $ enum (countBytes writeEnd)
        debug $ "sendResponse: whenEnum: " ++ Prelude.show bs ++ " bytes enumerated"

        return (x, bs-hl)


    --------------------------------------------------------------------------
    whenSendFile hs r f start = do
        -- guaranteed to have a content length here.
        joinIM $ (enumBS hs >. enumEof) writeEnd

        let !cl = fromJust $ rspContentLength r
        x <- liftIO $ onSendFile f start cl
        return (x, cl)


    --------------------------------------------------------------------------
    (major,minor) = rspHttpVersion rsp'


    --------------------------------------------------------------------------
    putHdrs hdrs =
        {-# SCC "putHdrs" #-}
        Prelude.mapM_ putHeader $ Map.toList hdrs
      where
        putHeader (k, ys) = Prelude.mapM_ (putOne k) ys

        putOne k y = do
            putByteString $ unCI k
            putByteString ": "
            putByteString y
            putByteString "\r\n"


    --------------------------------------------------------------------------
    noCL :: Response -> ServerMonad Response
    noCL r =
        {-# SCC "noCL" #-}
        do
            -- are we in HTTP/1.1?
            let sendChunked = (rspHttpVersion r) == (1,1)
            if sendChunked
              then do
                  let r' = setHeader "Transfer-Encoding" "chunked" r
                  let origE = rspBodyToEnum $ rspBody r

                  let e i = writeChunkedTransferEncoding i >>= origE

                  return $ r' { rspBody = Enum e }

              else do
                  -- HTTP/1.0 and no content-length? We'll have to close the
                  -- socket.
                  modify $! \s -> s { _forceConnectionClose = True }
                  return $ setHeader "Connection" "close" r


    --------------------------------------------------------------------------
    hasCL :: Int64 -> Response -> ServerMonad Response
    hasCL cl r =
        {-# SCC "hasCL" #-}
        do
            -- set the content-length header
            let r' = setHeader "Content-Length" (l2s $ show cl) r
            let b = case (rspBody r') of
                      (Enum e)       -> Enum (i e)
                      (SendFile f m) -> SendFile f m

            return $ r' { rspBody = b }

      where
        i :: forall z . Enumerator IO z -> Enumerator IO z
        i enum iter = enum (joinI $ takeExactly cl iter)


    --------------------------------------------------------------------------
    setFileSize :: FilePath -> Response -> ServerMonad Response
    setFileSize fp r =
        {-# SCC "setFileSize" #-}
        do
            fs <- liftM fromIntegral $ liftIO $ getFileSize fp
            return $ r { rspContentLength = Just fs }


    --------------------------------------------------------------------------
    handle304 :: Response -> Response
    handle304 r = setResponseBody (enumBS "") $
                  updateHeaders (Map.delete "Transfer-Encoding") $
                  setContentLength 0 r


    --------------------------------------------------------------------------
    fixupResponse :: Response -> ServerMonad Response
    fixupResponse r = {-# SCC "fixupResponse" #-} do
        let r' = deleteHeader "Content-Length" r

        let code = rspStatus r'

        let r'' = if code == 204 || code == 304
                   then handle304 r'
                   else r'

        r''' <- do
            z <- case (rspBody r'') of
                   (Enum _)                  -> return r''
                   (SendFile f Nothing)      -> setFileSize f r''
                   (SendFile _ (Just (s,e))) -> return $
                                                setContentLength (e-s) r''

            case (rspContentLength z) of
              Nothing   -> noCL z
              (Just sz) -> hasCL sz z

        -- HEAD requests cannot have bodies per RFC 2616 sec. 9.4
        if rqMethod req == HEAD
          then return $ deleteHeader "Transfer-Encoding"
                      $ r''' { rspBody = Enum $ enumBS "" }
          else return r'''


    --------------------------------------------------------------------------
    mkHeaderString :: Response -> ByteString
    mkHeaderString r = out
      where
        !out = {-# SCC "mkHeaderString" #-}
               S.concat $ L.toChunks $ runPut $ do
                   putByteString "HTTP/"
                   showp major
                   putAscii '.'
                   showp minor
                   putAscii ' '
                   showp $ rspStatus r
                   putAscii ' '
                   putByteString $ rspStatusReason r
                   putByteString "\r\n"
                   putHdrs $ headers r
                   putByteString "\r\n"
                   

------------------------------------------------------------------------------
checkConnectionClose :: (Int, Int) -> Headers -> ServerMonad ()
checkConnectionClose ver hdrs =
    -- For HTTP/1.1:
    --   if there is an explicit Connection: close, close the socket.
    -- For HTTP/1.0:
    --   if there is no explicit Connection: Keep-Alive, close the socket.
    if (ver == (1,1) && l == Just ["close"]) ||
       (ver == (1,0) && l /= Just ["keep-alive"])
       then modify $ \s -> s { _forceConnectionClose = True }
       else return ()
  where
    l  = liftM (map tl) $ Map.lookup "Connection" hdrs
    tl = S.map (c2w . toLower . w2c)


------------------------------------------------------------------------------
-- FIXME: whitespace-trim the values here.
toHeaders :: [(ByteString,ByteString)] -> Headers
toHeaders kvps = foldl' f Map.empty kvps'
  where
    kvps'     = map (first toCI . second (:[])) kvps
    f m (k,v) = Map.insertWith' (flip (++)) k v m


------------------------------------------------------------------------------
getFileSize :: FilePath -> IO FileOffset
getFileSize fp = liftM fileSize $ getFileStatus fp


------------------------------------------------------------------------------
l2s :: L.ByteString -> S.ByteString
l2s = S.concat . L.toChunks


------------------------------------------------------------------------------
toBS :: String -> ByteString
toBS = S.pack . map c2w


