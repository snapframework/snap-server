{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server where

------------------------------------------------------------------------------
import           Control.Arrow (first, second)
import           Control.Monad.State.Strict
import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Char
import           Data.CIByteString
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
import           Snap.Internal.Http.Types hiding (Enumerator)
import           Snap.Internal.Http.Parser
import           Snap.Iteratee hiding (foldl', head, take, FileOffset)
import qualified Snap.Iteratee as I

#ifdef LIBEV
import qualified Snap.Internal.Http.Server.LibevBackend as Backend
import           Snap.Internal.Http.Server.LibevBackend (debug)
#else
import qualified Snap.Internal.Http.Server.SimpleBackend as Backend
import           Snap.Internal.Http.Server.SimpleBackend (debug)
#endif

import           Snap.Internal.Http.Server.Date

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

data ServerState = ServerState
    { _forceConnectionClose  :: Bool
    , _localHostname         :: ByteString
    , _localAddress          :: ByteString
    , _localPort             :: Int
    , _remoteAddr            :: ByteString
    , _remotePort            :: Int
    , _logAccess             :: Request -> Response -> IO ()
    , _logError              :: ByteString -> IO ()
    }


------------------------------------------------------------------------------
runServerMonad :: ByteString                     -- ^ local host name
               -> ByteString                     -- ^ local ip address
               -> Int                            -- ^ local port
               -> ByteString                     -- ^ remote ip address
               -> Int                            -- ^ remote port
               -> (Request -> Response -> IO ()) -- ^ access log function
               -> (ByteString -> IO ())          -- ^ error log function
               -> ServerMonad a                  -- ^ monadic action to run
               -> Iteratee IO a
runServerMonad lh lip lp rip rp la le m = evalStateT m st
  where
    st = ServerState False lh lip lp rip rp la le



------------------------------------------------------------------------------
-- input/output


------------------------------------------------------------------------------
httpServe :: ByteString         -- ^ bind address, or \"*\" for all
          -> Int                -- ^ port to bind to
          -> ByteString         -- ^ local hostname (server name)
          -> Maybe FilePath     -- ^ path to the access log
          -> Maybe FilePath     -- ^ path to the error log
          -> ServerHandler      -- ^ handler procedure
          -> IO ()
httpServe bindAddress bindPort localHostname alogPath elogPath handler =
    withLoggers alogPath elogPath
                (\(alog, elog) -> spawnAll alog elog)

  where
    spawnAll alog elog = {-# SCC "httpServe/spawnAll" #-} do
        logE elog $ S.concat [ "Server.httpServe: START ("
                             , Backend.name, ")"]
        let n = numCapabilities
        bracket (spawn n)
                (\xs -> do
                     logE elog "Server.httpServe: SHUTDOWN"
                     mapM_ (Backend.stop . fst) xs
                     logE elog "Server.httpServe: BACKEND STOPPED")
                (runAll alog elog)


    runAll alog elog xs = {-# SCC "httpServe/runAll" #-} do
        mapM_ f $ xs `zip` [0..]
        mapM_ (takeMVar . snd) xs
      where
        f ((backend,mvar),cpu) =
            forkOnIO cpu $ do
                labelMe $ map w2c $ S.unpack $
                          S.concat ["accThread ", l2s $ show cpu]
                (try $ (goooo alog elog backend cpu)) :: IO (Either SomeException ())
                putMVar mvar ()

    goooo alog elog backend cpu =
        {-# SCC "httpServe/goooo" #-}
        let loop = go alog elog backend cpu >> loop
        in loop

    maybeSpawnLogger = maybe (return Nothing) $ (liftM Just) . newLogger

    withLoggers afp efp =
        bracket (do alog <- maybeSpawnLogger afp
                    elog <- maybeSpawnLogger efp
                    return (alog, elog))
                (\(alog, elog) -> do
                    threadDelay 1000000
                    maybe (return ()) stopLogger alog
                    maybe (return ()) stopLogger elog)

    labelMe :: String -> IO ()
    labelMe s = do
        tid <- myThreadId
        labelThread tid s

    spawn n = do
        sock <- Backend.bindIt bindAddress bindPort
        backends <- mapM (Backend.new sock) $ [0..(n-1)]
        mvars <- replicateM n newEmptyMVar

        return (backends `zip` mvars)


    runOne alog elog backend cpu =
        Backend.withConnection backend cpu $ \conn ->
          {-# SCC "httpServe/runOne" #-} do
            debug "Server.httpServe.runOne: entered"
            let readEnd = Backend.getReadEnd conn
            let writeEnd = Backend.getWriteEnd conn

            let raddr = Backend.getRemoteAddr conn
            let rport = Backend.getRemotePort conn
            let laddr = Backend.getLocalAddr conn
            let lport = Backend.getLocalPort conn

            runHTTP localHostname laddr lport raddr rport
                    alog elog readEnd writeEnd
                    (Backend.sendFile conn)
                    (Backend.tickleTimeout conn) handler

            debug "Server.httpServe.runHTTP: finished"


    go alog elog backend cpu = runOne alog elog backend cpu
        `catches`
        [ Handler $ \(_ :: Backend.TimeoutException) -> return ()

        , Handler $ \(e :: AsyncException) -> do
              logE elog $
                   S.concat [ "Server.httpServe.go: got async exception, "
                            , "terminating:\n", bshow e ]
              throwIO e

        , Handler $ \(e :: Backend.BackendTerminatedException) -> do
              logE elog $ "Server.httpServe.go: got backend terminated, waiting for cleanup"
              throwIO e

        , Handler $ \(e :: IOException) -> do
              logE elog $ S.concat [ "Server.httpServe.go: got io exception: "
                                   , bshow e ]

        , Handler $ \(e :: SomeException) -> do
              logE elog $ S.concat [ "Server.httpServe.go: got someexception: "
                                   , bshow e ] ]

------------------------------------------------------------------------------
debugE :: (MonadIO m) => ByteString -> m ()
debugE s = debug $ "Server: " ++ (map w2c $ S.unpack s)


------------------------------------------------------------------------------
logE :: Maybe Logger -> ByteString -> IO ()
logE elog = maybe debugE (\l s -> debugE s >> logE' l s) elog

logE' :: Logger -> ByteString -> IO ()
logE' logger s = (timestampedLogEntry s) >>= logMsg logger


bshow :: (Prelude.Show a) => a -> ByteString
bshow = toBS . Prelude.show

------------------------------------------------------------------------------
logA ::Maybe Logger -> Request -> Response -> IO ()
logA alog = maybe (\_ _ -> return ()) logA' alog

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
runHTTP :: ByteString                    -- ^ local host name
        -> ByteString                    -- ^ local ip address
        -> Int                           -- ^ local port
        -> ByteString                    -- ^ remote ip address
        -> Int                           -- ^ remote port
        -> Maybe Logger                  -- ^ access logger
        -> Maybe Logger                  -- ^ error logger
        -> Enumerator IO ()              -- ^ read end of socket
        -> Iteratee IO ()                -- ^ write end of socket
        -> (FilePath -> Int64 -> IO ())  -- ^ sendfile end
        -> IO ()                         -- ^ timeout tickler
        -> ServerHandler                 -- ^ handler procedure
        -> IO ()
runHTTP lh lip lp rip rp alog elog
        readEnd writeEnd onSendFile tickle handler =
    go `catches` [ Handler $ \(e :: AsyncException) -> do
                       throwIO e

                 , Handler $ \(_ :: Backend.TimeoutException) -> return ()

                 , Handler $ \(e :: SomeException) ->
                       logE elog $ S.concat [ logPrefix , bshow e ] ]

  where
    logPrefix = S.concat [ "[", rip, "]: error: " ]

    go = do
        buf <- mkIterateeBuffer
        let iter = runServerMonad lh lip lp rip rp (logA alog) (logE elog) $
                                  httpSession writeEnd buf onSendFile tickle
                                  handler
        readEnd iter >>= run


------------------------------------------------------------------------------
sERVER_HEADER :: [ByteString]
sERVER_HEADER = [S.concat ["Snap/", snapServerVersion]]

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
            -> (FilePath -> Int64 -> IO ())  -- ^ sendfile continuation
            -> IO ()                         -- ^ timeout tickler
            -> ServerHandler                 -- ^ handler procedure
            -> ServerMonad ()
httpSession writeEnd' ibuf onSendFile tickle handler = do

    (writeEnd, cancelBuffering) <-
        liftIO $ I.unsafeBufferIterateeWithBuffer ibuf writeEnd'

    -- (writeEnd, cancelBuffering) <- liftIO $ I.bufferIteratee writeEnd'
    let killBuffer = writeIORef cancelBuffering True


    liftIO $ debug "Server.httpSession: entered"
    mreq  <- receiveRequest
    -- successfully got a request, so restart timer
    liftIO tickle

    case mreq of
      (Just req) -> do
          liftIO $ debug $ "got request: " ++
                           Prelude.show (rqMethod req) ++
                           " " ++ SC.unpack (rqURI req) ++
                           " " ++ Prelude.show (rqVersion req)
          logerr <- gets _logError
          (req',rspOrig) <- lift $ handler logerr req
          let rspTmp = rspOrig { rspHttpVersion = rqVersion req }
          checkConnectionClose (rspHttpVersion rspTmp) (rspHeaders rspTmp)

          cc <- gets _forceConnectionClose
          let rsp = if cc
                      then (setHeader "Connection" "close" rspTmp)
                      else rspTmp


          liftIO $ debug "Server.httpSession: handled, skipping request body"
          srqEnum <- liftIO $ readIORef $ rqBody req'
          let (SomeEnumerator rqEnum) = srqEnum
          lift $ joinIM $ rqEnum skipToEof
          liftIO $ debug "Server.httpSession: request body skipped, sending response"

          date <- liftIO getDateString
          let ins = (Map.insert "Date" [date] . Map.insert "Server" sERVER_HEADER)
          let rsp' = updateHeaders ins rsp
          (bytesSent,_) <- sendResponse rsp' writeEnd ibuf killBuffer onSendFile

          liftIO . debug $ "Server.httpSession: sent " ++
                           (Prelude.show bytesSent) ++ " bytes"

          maybe (logAccess req rsp')
                (\_ -> logAccess req $ setContentLength bytesSent rsp')
                (rspContentLength rsp')

          if cc
             then return ()
             else httpSession writeEnd' ibuf onSendFile tickle handler

      Nothing -> return ()

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
          then liftIO $ writeIORef (rqBody req)
                                   (SomeEnumerator readChunkedTransferEncoding)
          else maybe noContentLength hasContentLength mbCL

      where
        isChunked = maybe False
                          ((== ["chunked"]) . map toCI)
                          (Map.lookup "transfer-encoding" hdrs)

        hasContentLength :: Int -> ServerMonad ()
        hasContentLength l = do
            liftIO $ writeIORef (rqBody req)
                         (SomeEnumerator e)
          where
            e :: Enumerator IO a
            e = return . joinI . I.take l

        noContentLength :: ServerMonad ()
        noContentLength =
            liftIO $ writeIORef (rqBody req)
                (SomeEnumerator $ return . joinI . I.take 0 )


        hdrs = rqHeaders req
        mbCL = Map.lookup "content-length" hdrs >>= return . Cvt.int . head


    parseForm :: Request -> ServerMonad Request
    parseForm req =
        {-# SCC "receiveRequest/parseForm" #-} if doIt then getIt else return req
      where
        doIt = mbCT == Just "application/x-www-form-urlencoded"
        mbCT = liftM head $ Map.lookup "content-type" (rqHeaders req)

        maximumPOSTBodySize :: Int64
        maximumPOSTBodySize = 10*1024*1024

        getIt :: ServerMonad Request
        getIt = {-# SCC "receiveRequest/parseForm/getIt" #-} do
            senum <- liftIO $ readIORef $ rqBody req
            let (SomeEnumerator enum) = senum
            let i = joinI $ takeNoMoreThan maximumPOSTBodySize stream2stream
            iter <- liftIO $ enum i
            body <- liftM unWrap $ lift iter
            let newParams = parseUrlEncoded body
            liftIO $ writeIORef (rqBody req)
                         (SomeEnumerator $ enumBS body)
            return $ req { rqParams = rqParams req `mappend` newParams }


    toRequest (IRequest method uri version kvps) =
        {-# SCC "receiveRequest/toRequest" #-} do
            localAddr     <- gets _localAddress
            localPort     <- gets _localPort
            remoteAddr    <- gets _remoteAddr
            remotePort    <- gets _remotePort
            localHostname <- gets _localHostname

            let (serverName, serverPort) = fromMaybe
                                             (localHostname, localPort)
                                             (liftM (parseHost . head)
                                                    (Map.lookup "host" hdrs))

            -- will override in "setEnumerator"
            enum <- liftIO $ newIORef $ SomeEnumerator return


            return $ Request serverName
                             serverPort
                             remoteAddr
                             remotePort
                             localAddr
                             localPort
                             localHostname
                             isSecure
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

        isSecure        = False

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
sendResponse :: Response
             -> Iteratee IO a
             -> ForeignPtr CChar
             -> IO ()
             -> (FilePath -> Int64 -> IO a)
             -> ServerMonad (Int64, a)
sendResponse rsp' writeEnd ibuf killBuffering onSendFile = do
    rsp <- fixupResponse rsp'
    let !headerString = mkHeaderString rsp

    (!x,!bs) <- case (rspBody rsp) of
                  (Enum e)     -> liftIO $ whenEnum headerString e
                  (SendFile f) -> liftIO $ whenSendFile headerString rsp f

    return $! (bs,x)

  where
    whenEnum hs e = do
        let enum = enumBS hs >. e
        let hl = fromIntegral $ S.length hs

        (x,bs) <- liftIO $ enum (countBytes writeEnd) >>= run

        return (x, bs-hl)

    whenSendFile hs r f = do
        -- guaranteed to have a content length here.
        enumBS hs writeEnd >>= run

        let !cl = fromJust $ rspContentLength r
        x <- onSendFile f cl
        return (x, cl)

    (major,minor) = rspHttpVersion rsp'


    fmtHdrs hdrs =
        {-# SCC "fmtHdrs" #-}
        concat xs
      where
        xs = map f $ Map.toList hdrs

        f (k, ys) = map (g k) ys

        g k y = S.concat [ unCI k, ": ", y, "\r\n" ]


    noCL :: Response -> ServerMonad Response
    noCL r =
        {-# SCC "noCL" #-}
        do
            -- are we in HTTP/1.1?
            let sendChunked = (rspHttpVersion r) == (1,1)
            if sendChunked
              then do
                  liftIO $ killBuffering
                  let r' = setHeader "Transfer-Encoding" "chunked" r
                  let e  = writeChunkedTransferEncoding ibuf $
                           rspBodyToEnum $ rspBody r
                  return $ r' { rspBody = Enum e }

              else do
                  -- HTTP/1.0 and no content-length? We'll have to close the
                  -- socket.
                  modify $! \s -> s { _forceConnectionClose = True }
                  return $ setHeader "Connection" "close" r


    hasCL :: Int64 -> Response -> ServerMonad Response
    hasCL cl r =
        {-# SCC "hasCL" #-}
        do
            -- set the content-length header
            let r' = setHeader "Content-Length" (l2s $ show cl) r
            let b = case (rspBody r') of
                      (Enum e)     -> Enum (i e)
                      (SendFile f) -> SendFile f

            return $ r' { rspBody = b }

      where
        i :: Enumerator IO a -> Enumerator IO a
        i enum iter = enum (joinI $ takeExactly cl iter)


    setFileSize :: FilePath -> Response -> ServerMonad Response
    setFileSize fp r =
        {-# SCC "setFileSize" #-}
        do
            fs <- liftM fromIntegral $ liftIO $ getFileSize fp
            return $ r { rspContentLength = Just fs }


    handle304 :: Response -> Response
    handle304 r = setResponseBody (enumBS "") $
                  updateHeaders (Map.delete "Transfer-Encoding") $
                  setContentLength 0 r

    fixupResponse :: Response -> ServerMonad Response
    fixupResponse r =
        {-# SCC "fixupResponse" #-}
        do
            let r' = updateHeaders (Map.delete "Content-Length") r

            let code = rspStatus r'

            let r'' = if code == 204 || code == 304
                       then handle304 r'
                       else r'

            r''' <- case (rspBody r'') of
                     (Enum _)     -> return r''
                     (SendFile f) -> setFileSize f r''
            case (rspContentLength r''') of
              Nothing   -> noCL r'''
              (Just sz) -> hasCL sz r'''


    bsshow = l2s . show


    mkHeaderString :: Response -> ByteString
    mkHeaderString r =
        {-# SCC "mkHeaderString" #-}
        S.concat $ concat [hl, hdr, eol]
      where
        hl = [ "HTTP/"
             , bsshow major
             , "."
             , bsshow minor
             , " "
             , bsshow $ rspStatus r
             , " "
             , rspStatusReason r
             , "\r\n" ]

        hdr = fmtHdrs $ headers r

        eol = ["\r\n"]


------------------------------------------------------------------------------
checkConnectionClose :: (Int, Int) -> Headers -> ServerMonad ()
checkConnectionClose ver hdrs =
    -- For HTTP/1.1:
    --   if there is an explicit Connection: close, close the socket.
    -- For HTTP/1.0:
    --   if there is no explicit Connection: Keep-Alive, close the socket.
    if (ver == (1,1) && l == Just ["close"]) ||
       (ver == (1,0) && l /= Just ["Keep-Alive"])
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


l2s :: L.ByteString -> S.ByteString
l2s = S.concat . L.toChunks


toBS :: String -> ByteString
toBS = S.pack . map c2w
