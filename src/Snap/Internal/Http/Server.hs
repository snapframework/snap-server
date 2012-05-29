{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import           Blaze.ByteString.Builder.Enumerator
import           Blaze.ByteString.Builder.HTTP
import           Control.Arrow (first, second)
import           Control.Monad.CatchIO hiding ( bracket
                                              , catches
                                              , finally
                                              , Handler
                                              )
import qualified Control.Monad.CatchIO as CatchIO
import           Control.Monad.State.Strict
import           Control.Exception hiding (catch, throw)
import           Data.Char
import qualified Data.CaseInsensitive as CI
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Nums.Careless.Int as Cvt
import           Data.Enumerator.Internal
import           Data.Int
import           Data.IORef
import           Data.List (foldl')
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import           Data.Monoid
import           Data.Time
import           Data.Typeable
import           Data.Version
import           GHC.Conc
import           Network.Socket (withSocketsDo, Socket)
import           Prelude hiding (catch)
import           System.PosixCompat.Files hiding (setFileSize)
import           System.Posix.Types (FileOffset)
import           System.Locale
------------------------------------------------------------------------------
import           System.FastLogger (timestampedLogEntry, combinedLogEntry)
import           Snap.Internal.Http.Types
import           Snap.Internal.Debug
import           Snap.Internal.Exceptions (EscapeHttpException (..))
import           Snap.Internal.Http.Parser
import           Snap.Internal.Http.Server.Date

import           Snap.Internal.Http.Server.Backend
import           Snap.Internal.Http.Server.HttpPort
import qualified Snap.Internal.Http.Server.TLS as TLS
import           Snap.Internal.Http.Server.SimpleBackend
import           Snap.Internal.Http.Server.LibevBackend

import           Snap.Internal.Iteratee.Debug
import           Snap.Iteratee hiding (head, take, map)
import qualified Snap.Iteratee as I

import           Snap.Types.Headers (Headers)
import qualified Snap.Types.Headers as H

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
                  -> ((Int -> Int) -> IO ())
                  -> Request
                  -> Iteratee ByteString IO (Request,Response)


------------------------------------------------------------------------------
type ServerMonad = StateT ServerState (Iteratee ByteString IO)


------------------------------------------------------------------------------
data ListenPort =
    -- (bind address, port)
    HttpPort  ByteString Int |
    -- (bind address, port, path to certificate, path to key)
    HttpsPort ByteString Int FilePath FilePath

------------------------------------------------------------------------------
instance Show ListenPort where
    show (HttpPort  b p    ) =
        concat [ "http://" , SC.unpack b, ":", show p, "/" ]

    show (HttpsPort b p _ _) =
        concat [ "https://", SC.unpack b, ":", show p, "/" ]


------------------------------------------------------------------------------
data EventLoopType = EventLoopSimple
                   | EventLoopLibEv
  deriving (Show)


------------------------------------------------------------------------------
-- This exception will be thrown if we decided to terminate the request before
-- running the user handler.
data TerminatedBeforeHandlerException = TerminatedBeforeHandlerException
  deriving (Show, Typeable)
instance Exception TerminatedBeforeHandlerException


-- We throw this if we get an exception that escaped from the user handler.
data ExceptionAlreadyCaught = ExceptionAlreadyCaught
  deriving (Show, Typeable)
instance Exception ExceptionAlreadyCaught


------------------------------------------------------------------------------
defaultEvType :: EventLoopType
#ifdef LIBEV
defaultEvType = EventLoopLibEv
#else
defaultEvType = EventLoopSimple
#endif


------------------------------------------------------------------------------
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
               -> Iteratee ByteString IO a
runServerMonad lh s la le m = evalStateT m st
  where
    st = ServerState False lh s la le


------------------------------------------------------------------------------
-- input/output


------------------------------------------------------------------------------
httpServe :: Int                         -- ^ default timeout
          -> [ListenPort]                -- ^ ports to listen on
          -> Maybe EventLoopType         -- ^ Specify a given event loop,
                                         --   otherwise a default is picked
          -> ByteString                  -- ^ local hostname (server name)
          -> Maybe (ByteString -> IO ()) -- ^ access log action
          -> Maybe (ByteString -> IO ()) -- ^ error log action
          -> ([Socket] -> IO ())         -- ^ initialisation
          -> ServerHandler               -- ^ handler procedure
          -> IO ()
httpServe defaultTimeout ports mevType localHostname alog' elog' initial
          handler = withSocketsDo $ spawnAll alog' elog'
  where
    --------------------------------------------------------------------------
    spawnAll alog elog = {-# SCC "httpServe/spawnAll" #-} do

        let evType = maybe defaultEvType id mevType

        logE elog $ S.concat [ "Server.httpServe: START ("
                             , toBS $ show evType, ")"]

        let isHttps p = case p of { (HttpsPort _ _ _ _) -> True; _ -> False;}
        let initHttps = foldr (\p b -> b || isHttps p) False ports

        if initHttps
            then TLS.initTLS
            else return ()

        nports <- mapM bindPort ports
        let socks = map (\x -> case x of ListenHttp s -> s; ListenHttps s _ -> s) nports

        (runEventLoop evType defaultTimeout nports numCapabilities (logE elog) (initial socks)
                    $ runHTTP defaultTimeout alog elog handler localHostname)
          `finally` do
            logE elog "Server.httpServe: SHUTDOWN"

            if initHttps
                then TLS.stopTLS
                else return ()

            logE elog "Server.httpServe: BACKEND STOPPED"

    --------------------------------------------------------------------------
    bindPort (HttpPort  baddr port         ) = bindHttp  baddr port
    bindPort (HttpsPort baddr port cert key) =
        TLS.bindHttps baddr port cert key


    --------------------------------------------------------------------------
    runEventLoop EventLoopSimple       = simpleEventLoop
    runEventLoop EventLoopLibEv        = libEvEventLoop


------------------------------------------------------------------------------
debugE :: (MonadIO m) => ByteString -> m ()
debugE s = debug $ "Server: " ++ (map w2c $ S.unpack s)


------------------------------------------------------------------------------
logE :: Maybe (ByteString -> IO ()) -> ByteString -> IO ()
logE elog = maybe debugE (\l s -> debugE s >> logE' l s) elog


------------------------------------------------------------------------------
logE' :: (ByteString -> IO ()) -> ByteString -> IO ()
logE' logger s = (timestampedLogEntry s) >>= logger


------------------------------------------------------------------------------
bshow :: (Show a) => a -> ByteString
bshow = toBS . show


------------------------------------------------------------------------------
logA :: Maybe (ByteString -> IO ()) -> Request -> Response -> IO ()
logA alog = maybe (\_ _ -> return ()) logA' alog


------------------------------------------------------------------------------
logA' :: (ByteString -> IO ()) -> Request -> Response -> IO ()
logA' logger req rsp = do
    let hdrs      = rqHeaders req
    let host      = rqRemoteAddr req
    let user      = Nothing -- TODO we don't do authentication yet
    let (v, v')   = rqVersion req
    let ver       = S.concat [ "HTTP/", bshow v, ".", bshow v' ]
    let method    = toBS $ show (rqMethod req)
    let reql      = S.intercalate " " [ method, rqURI req, ver ]
    let status    = rspStatus rsp
    let cl        = rspContentLength rsp
    let referer   = maybe Nothing (Just . head) $ H.lookup "referer" hdrs
    let userAgent = maybe "-" head $ H.lookup "user-agent" hdrs

    msg <- combinedLogEntry host user reql status cl referer userAgent
    logger msg


------------------------------------------------------------------------------
runHTTP :: Int                           -- ^ default timeout
        -> Maybe (ByteString -> IO ())   -- ^ access logger
        -> Maybe (ByteString -> IO ())   -- ^ error logger
        -> ServerHandler                 -- ^ handler procedure
        -> ByteString                    -- ^ local host name
        -> SessionInfo                   -- ^ session port information
        -> Enumerator ByteString IO ()   -- ^ read end of socket
        -> Iteratee ByteString IO ()     -- ^ write end of socket
        -> (FilePath -> Int64 -> Int64 -> IO ())
                                         -- ^ sendfile end
        -> ((Int -> Int) -> IO ())       -- ^ timeout tickler
        -> IO ()
runHTTP defaultTimeout alog elog handler lh sinfo readEnd writeEnd onSendFile
        tickle =
    go `catches` [ Handler $ \(_ :: TerminatedBeforeHandlerException) -> do
                       return ()
                 , Handler $ \(_ :: ExceptionAlreadyCaught) -> do
                       return ()
                 , Handler $ \(_ :: HttpParseException) -> do
                       return ()
                 , Handler $ \(e :: AsyncException) -> do
                       throwIO e
                 , Handler $ \(e :: SomeException) ->
                       logE elog $ toByteString $ lmsg e
                 ]

  where
    lmsg e = mconcat [ fromByteString "["
                     , fromShow $ remoteAddress sinfo
                     , fromByteString "]: "
                     , fromByteString "an exception escaped to toplevel:\n"
                     , fromShow e
                     ]

    go = do
        buf <- allocBuffer 16384
        let iter1 = runServerMonad lh sinfo (logA alog) (logE elog) $
                                   httpSession defaultTimeout writeEnd buf
                                               onSendFile tickle handler
        let iter = iterateeDebugWrapper "httpSession iteratee" iter1

        debug "runHTTP/go: prepping iteratee for start"

        step <- liftIO $ runIteratee iter

        debug "runHTTP/go: running..."
        run_ $ readEnd step
        debug "runHTTP/go: finished"


------------------------------------------------------------------------------
requestErrorMessage :: Request -> SomeException -> Builder
requestErrorMessage req e =
    mconcat [ fromByteString "During processing of request from "
            , fromByteString $ rqRemoteAddr req
            , fromByteString ":"
            , fromShow $ rqRemotePort req
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
sERVER_HEADER :: ByteString
sERVER_HEADER = S.concat ["Snap/", snapServerVersion]


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
httpSession :: Int
            -> Iteratee ByteString IO ()     -- ^ write end of socket
            -> Buffer                        -- ^ builder buffer
            -> (FilePath -> Int64 -> Int64 -> IO ())
                                             -- ^ sendfile continuation
            -> ((Int -> Int) -> IO ())       -- ^ timeout modifier
            -> ServerHandler                 -- ^ handler procedure
            -> ServerMonad ()
httpSession defaultTimeout writeEnd' buffer onSendFile tickle handler = do

    let writeEnd = iterateeDebugWrapper "writeEnd" writeEnd'

    debug "Server.httpSession: entered"
    mreq  <- receiveRequest writeEnd
    debug "Server.httpSession: receiveRequest finished"

    -- successfully got a request, so restart timer
    liftIO $ tickle (max defaultTimeout)

    case mreq of
      (Just req) -> do
          debug $ "Server.httpSession: got request: " ++
                   show (rqMethod req) ++
                   " " ++ SC.unpack (rqURI req) ++
                   " " ++ show (rqVersion req)

          -- check for Expect: 100-continue
          checkExpect100Continue req writeEnd

          logerr <- gets _logError

          (req',rspOrig) <- (lift $ handler logerr tickle req)
              `CatchIO.catches`
                  [ CatchIO.Handler $ escapeHttpCatch
                  , CatchIO.Handler $ errCatch "user handler" req
                  ]

          debug $ "Server.httpSession: finished running user handler"

          let rspTmp = rspOrig { rspHttpVersion = rqVersion req }
          checkConnectionClose (rspHttpVersion rspTmp) (rspHeaders rspTmp)

          cc <- gets _forceConnectionClose
          let rsp = if cc
                      then (setHeader "Connection" "close" rspTmp)
                      else rspTmp

          debug "Server.httpSession: handled, skipping request body"

          if rspTransformingRqBody rsp
             then debug $
                      "Server.httpSession: not skipping " ++
                      "request body, transforming."
             else do
               srqEnum <- liftIO $ readIORef $ rqBody req'
               let (SomeEnumerator rqEnum) = srqEnum

               skipStep <- (liftIO $ runIteratee $ iterateeDebugWrapper
                               "httpSession/skipToEof" skipToEof)
                           `catch` errCatch "skipping request body" req
               (lift $ rqEnum skipStep) `catch`
                      errCatch "skipping request body" req

          debug $ "Server.httpSession: request body skipped, " ++
                           "sending response"

          date <- liftIO getDateString
          let insHeader = H.set "Server" sERVER_HEADER
          let ins = H.set "Date" date .
                    if isJust (getHeader "Server" rsp)
                       then id
                       else insHeader
          let rsp' = updateHeaders ins rsp
          (bytesSent,_) <- sendResponse req rsp' buffer writeEnd onSendFile
                           `catch` errCatch "sending response" req

          debug $ "Server.httpSession: sent " ++
                  (show bytesSent) ++ " bytes"

          maybe (logAccess req rsp')
                (\_ -> logAccess req $ setContentLength bytesSent rsp')
                (rspContentLength rsp')

          if cc
             then do
                 debug $ "httpSession: Connection: Close, harikari"
                 liftIO $ myThreadId >>= killThread
             else httpSession defaultTimeout writeEnd' buffer onSendFile
                              tickle handler

      Nothing -> do
          debug $ "Server.httpSession: parser did not produce a " ++
                           "request, ending session"
          return ()

  where
    escapeHttpCatch :: EscapeHttpException -> ServerMonad a
    escapeHttpCatch (EscapeHttpException escapeIter) = do
        lift $ escapeIter tickle writeEnd'
        throw ExceptionAlreadyCaught

    errCatch :: ByteString -> Request -> SomeException -> ServerMonad a
    errCatch phase req e = do
        logError $ toByteString $
          mconcat [ fromByteString "httpSession caught an exception during "
                  , fromByteString phase
                  , fromByteString " phase:\n"
                  , requestErrorMessage req e
                  ]
        throw ExceptionAlreadyCaught


------------------------------------------------------------------------------
checkExpect100Continue :: Request
                       -> Iteratee ByteString IO ()
                       -> ServerMonad ()
checkExpect100Continue req writeEnd = do
    let mbEx = getHeaders "Expect" req

    maybe (return ())
          (\l -> if elem "100-continue" l then go else return ())
          mbEx

  where
    go = do
        let (major,minor) = rqVersion req
        let hl = mconcat [ fromByteString "HTTP/"
                         , fromShow major
                         , fromWord8 $ c2w '.'
                         , fromShow minor
                         , fromByteString " 100 Continue\r\n\r\n"
                         ]
        liftIO $ runIteratee
                   ((enumBS (toByteString hl) >==> enumEOF) $$ writeEnd)
        return ()


------------------------------------------------------------------------------
return411 :: Request
          -> Iteratee ByteString IO ()
          -> ServerMonad a
return411 req writeEnd = do
    go
    liftIO $ throwIO $ TerminatedBeforeHandlerException

  where
    go = do
        let (major,minor) = rqVersion req
        let hl = mconcat [ fromByteString "HTTP/"
                         , fromShow major
                         , fromWord8 $ c2w '.'
                         , fromShow minor
                         , fromByteString " 411 Length Required\r\n\r\n"
                         , fromByteString "411 Length Required\r\n"
                         ]
        liftIO $ runIteratee
                   ((enumBS (toByteString hl) >==> enumEOF) $$ writeEnd)
        return ()


------------------------------------------------------------------------------
receiveRequest :: Iteratee ByteString IO () -> ServerMonad (Maybe Request)
receiveRequest writeEnd = do
    debug "receiveRequest: entered"
    mreq <- {-# SCC "receiveRequest/parseRequest" #-} lift $
            iterateeDebugWrapper "parseRequest" $
            joinI' $ takeNoMoreThan maxHeadersSize $$ parseRequest
    debug "receiveRequest: parseRequest returned"

    case mreq of
      (Just ireq) -> do
          req' <- toRequest ireq
          setEnumerator req'
          req  <- parseForm req'
          checkConnectionClose (rqVersion req) (rqHeaders req)
          return $! Just req

      Nothing     -> return Nothing

  where
    --------------------------------------------------------------------------
    -- TODO(gdc): make this a policy decision (expose in
    -- Snap.Http.Server.Config)
    maxHeadersSize = 256 * 1024

    --------------------------------------------------------------------------
    -- check: did the client specify "transfer-encoding: chunked"? then we
    -- have to honor that.
    --
    -- otherwise: check content-length header. if set: only take N bytes from
    -- the read end of the socket
    --
    -- if no content-length and no chunked encoding, enumerate the entire
    -- socket and close afterwards
    setEnumerator :: Request -> ServerMonad ()
    setEnumerator req = {-# SCC "receiveRequest/setEnumerator" #-} do
        if isChunked
          then do
              debug $ "receiveRequest/setEnumerator: " ++
                               "input in chunked encoding"
              let e = joinI . readChunkedTransferEncoding
              liftIO $ writeIORef (rqBody req)
                                  (SomeEnumerator e)
          else maybe (noContentLength req) hasContentLength mbCL

      where
        isChunked = maybe False
                          ((== ["chunked"]) . map CI.mk)
                          (H.lookup "transfer-encoding" hdrs)

        hasContentLength :: Int64 -> ServerMonad ()
        hasContentLength len = do
            debug $ "receiveRequest/setEnumerator: " ++
                             "request had content-length " ++ show len
            liftIO $ writeIORef (rqBody req) (SomeEnumerator e)
            debug "receiveRequest/setEnumerator: body enumerator set"
          where
            e :: Enumerator ByteString IO a
            e st = do
                st' <- lift $
                       runIteratee $
                       iterateeDebugWrapper "rqBody iterator" $
                       returnI st

                joinI $ takeExactly len st'

        noContentLength :: Request -> ServerMonad ()
        noContentLength rq = do
            debug ("receiveRequest/setEnumerator: " ++
                   "request did NOT have content-length")

            when (rqMethod rq == POST || rqMethod rq == PUT) $
                 return411 req writeEnd

            let enum = SomeEnumerator $
                       iterateeDebugWrapper "noContentLength" .
                       joinI . I.take 0
            liftIO $ writeIORef (rqBody rq) enum
            debug "receiveRequest/setEnumerator: body enumerator set"


        hdrs = rqHeaders req
        mbCL = H.lookup "content-length" hdrs >>= return . Cvt.int . head


    --------------------------------------------------------------------------
    parseForm :: Request -> ServerMonad Request
    parseForm req = {-# SCC "receiveRequest/parseForm" #-}
        if doIt then getIt else return req
      where
        mbCT   = liftM head $ H.lookup "content-type" (rqHeaders req)
        trimIt = fst . SC.spanEnd isSpace . SC.takeWhile (/= ';')
                     . SC.dropWhile isSpace
        mbCT'  = liftM trimIt mbCT
        doIt   = mbCT' == Just "application/x-www-form-urlencoded"

        maximumPOSTBodySize :: Int64
        maximumPOSTBodySize = 10*1024*1024

        getIt :: ServerMonad Request
        getIt = {-# SCC "receiveRequest/parseForm/getIt" #-} do
            debug "parseForm: got application/x-www-form-urlencoded"
            debug "parseForm: reading POST body"
            senum <- liftIO $ readIORef $ rqBody req
            let (SomeEnumerator enum) = senum
            consumeStep <- liftIO $ runIteratee consume
            step <- liftIO $
                    runIteratee $
                    joinI $ takeNoMoreThan maximumPOSTBodySize consumeStep
            body <- liftM S.concat $ lift $ enum step
            let newParams = parseUrlEncoded body

            debug "parseForm: stuffing 'enumBS body' into request"

            let e = enumBS body >==> I.joinI . I.take 0

            let e' = \st -> do
                let ii = iterateeDebugWrapper "regurgitate body" (returnI st)
                st' <- lift $ runIteratee ii
                e st'

            liftIO $ writeIORef (rqBody req) $ SomeEnumerator e'
            return $! req { rqParams = Map.unionWith (++) (rqParams req)
                                         newParams
                          , rqPostParams = newParams
                          }


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
                                                    (H.lookup "host" hdrs))

            -- will override in "setEnumerator"
            enum <- liftIO $ newIORef $ SomeEnumerator (enumBS "")

            return $! Request serverName
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
                              pathInfo
                              contextPath
                              uri
                              queryString
                              params
                              params
                              Map.empty

      where
        dropLeadingSlash s = maybe s f mbS
          where
            f (a,s') = if a == c2w '/' then s' else s
            mbS = S.uncons s

        hdrs            = toHeaders kvps

        mbContentLength = liftM (Cvt.int . head) $
                          H.lookup "content-length" hdrs

        cookies         = concat $
                          maybe []
                                (catMaybes . map parseCookie)
                                (H.lookup "cookie" hdrs)

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
             -> Buffer
             -> Iteratee ByteString IO a             -- ^ iteratee write end
             -> (FilePath -> Int64 -> Int64 -> IO a) -- ^ function to call on
                                                     -- sendfile
             -> ServerMonad (Int64, a)
sendResponse req rsp' buffer writeEnd' onSendFile = do
    let rsp'' = renderCookies rsp'
    rsp <- fixupResponse rsp''
    let (!headerString,!hlen) = mkHeaderBuilder rsp
    let writeEnd = fixCLIteratee hlen rsp writeEnd'

    (!x,!bs) <-
        case (rspBody rsp) of
          (Enum e)             -> lift $ whenEnum writeEnd headerString hlen
                                                  rsp e
          (SendFile f Nothing) -> lift $
                                  whenSendFile writeEnd headerString rsp f 0
          (SendFile f (Just (st,_))) ->
              lift $ whenSendFile writeEnd headerString rsp f st

    debug "sendResponse: response sent"

    return $! (bs,x)

  where
    --------------------------------------------------------------------------
    whenEnum :: Iteratee ByteString IO a
             -> Builder
             -> Int
             -> Response
             -> (forall x . Enumerator Builder IO x)
             -> Iteratee ByteString IO (a,Int64)
    whenEnum writeEnd hs hlen rsp e = do
        -- "enum" here has to be run in the context of the READ iteratee, even
        -- though it's writing to the output, because we may be transforming
        -- the input. That's why we check if we're transforming the request
        -- body here, and if not, send EOF to the write end; so that it
        -- doesn't join up with the read iteratee and try to get more data
        -- from the socket.
        let eBuilder = enumBuilder hs >==> e
        let enum = if rspTransformingRqBody rsp
                     then eBuilder
                     else eBuilder >==>
                          mapEnum toByteString fromByteString
                                  (joinI . I.take 0)

        debug $ "sendResponse: whenEnum: enumerating bytes"

        outstep <- lift $ runIteratee $
                   iterateeDebugWrapper "countBytes writeEnd" $
                   countBytes writeEnd

        let bufferFunc = if getBufferingMode rsp
                           then unsafeBuilderToByteString (return buffer)
                           else I.map (toByteString . (`mappend` flush))

        (x,bs) <- mapIter fromByteString toByteString
                          (enum $$ joinI $ bufferFunc outstep)

        debug $ "sendResponse: whenEnum: " ++ show bs ++
                " bytes enumerated"

        return (x, bs - fromIntegral hlen)


    --------------------------------------------------------------------------
    whenSendFile :: Iteratee ByteString IO a  -- ^ write end
                 -> Builder       -- ^ headers
                 -> Response
                 -> FilePath      -- ^ file to send
                 -> Int64         -- ^ start byte offset
                 -> Iteratee ByteString IO (a,Int64)
    whenSendFile writeEnd hs r f start = do
        -- Guaranteed to have a content length here. Sending EOF through to
        -- the write end guarantees that we flush the buffer before we send
        -- the file with sendfile().
        lift $ runIteratee ((enumBuilder hs >==> enumEOF) $$
                            unsafeBuilderToByteString (return buffer)
                              $$ writeEnd)

        let !cl = fromJust $ rspContentLength r
        x <- liftIO $ onSendFile f start cl
        return (x, cl)


    --------------------------------------------------------------------------
    (major,minor) = rspHttpVersion rsp'


    --------------------------------------------------------------------------
    buildHdrs :: Headers -> (Builder,Int)
    buildHdrs hdrs =
        {-# SCC "buildHdrs" #-}
        H.fold f (mempty,0) hdrs
      where
        f (!b,!len) !k !ys =
            let (!b',len') = h k ys
            in (b `mappend` b', len+len')

        crlf = fromByteString "\r\n"

        doOne pre plen (b,len) y = ( mconcat [ b
                                             , pre
                                             , fromByteString y
                                             , crlf ]
                                   , len + plen + 2 + S.length y )

        h k ys = foldl' (doOne kb klen) (mempty,0) ys
          where
            k'      = CI.original k
            kb      = fromByteString k' `mappend` fromByteString ": "
            klen    = S.length k' + 2


    --------------------------------------------------------------------------
    noCL :: Response
         -> ServerMonad Response
    noCL r = {-# SCC "noCL" #-} do
        -- are we in HTTP/1.1?
        let sendChunked = (rspHttpVersion r) == (1,1)
        if sendChunked
          then do
              let r' = setHeader "Transfer-Encoding" "chunked" r
              let origE = rspBodyToEnum $ rspBody r

              let e = \i -> joinI $ origE $$ chunkIt i

              return $! r' { rspBody = Enum e }

          else do
              -- HTTP/1.0 and no content-length? We'll have to close the
              -- socket.
              modify $! \s -> s { _forceConnectionClose = True }
              return $! setHeader "Connection" "close" r

    --------------------------------------------------------------------------
    chunkIt :: forall x . Enumeratee Builder Builder IO x
    chunkIt = checkDone $ continue . step
      where
        step k EOF = k (Chunks [chunkedTransferTerminator]) >>== return
        step k (Chunks []) = continue $ step k
        step k (Chunks xs) = k (Chunks [chunkedTransferEncoding $ mconcat xs])
                             >>== chunkIt

    --------------------------------------------------------------------------
    fixCLIteratee :: Int                       -- ^ header length
                  -> Response                  -- ^ response
                  -> Iteratee ByteString IO a  -- ^ write end
                  -> Iteratee ByteString IO a
    fixCLIteratee hlen resp we = maybe we f mbCL
      where
        f cl = case rspBody resp of
                 (Enum _) -> joinI $ takeExactly (cl + fromIntegral hlen)
                                  $$ we
                 (SendFile _ _) -> we

        mbCL = rspContentLength resp

    --------------------------------------------------------------------------
    hasCL :: Int64
          -> Response
          -> ServerMonad Response
    hasCL cl r = {-# SCC "hasCL" #-}
        -- set the content-length header
        return $! setHeader "Content-Length" (toByteString $ fromShow cl) r


    --------------------------------------------------------------------------
    setFileSize :: FilePath -> Response -> ServerMonad Response
    setFileSize fp r =
        {-# SCC "setFileSize" #-}
        do
            fs <- liftM fromIntegral $ liftIO $ getFileSize fp
            return $! r { rspContentLength = Just fs }


    --------------------------------------------------------------------------
    handle304 :: Response -> Response
    handle304 r = setResponseBody (enumBuilder mempty) $
                  updateHeaders (H.delete "Transfer-Encoding") $
                  setContentLength 0 r


    --------------------------------------------------------------------------
    renderCookies :: Response -> Response
    renderCookies r = updateHeaders f r
      where
        f h = if null cookies
                then h
                else foldl' (\m v -> H.insert "Set-Cookie" v m) h cookies
        cookies = fmap cookieToBS . Map.elems $ rspCookies r


    --------------------------------------------------------------------------
    fixupResponse :: Response
                  -> ServerMonad Response
    fixupResponse r = {-# SCC "fixupResponse" #-} do
        let r' = deleteHeader "Content-Length" r
        let code = rspStatus r'
        let r'' = if code == 204 || code == 304
                   then handle304 r'
                   else r'

        r''' <- do
            z <- case rspBody r'' of
                   (Enum _)                  -> return r''
                   (SendFile f Nothing)      -> setFileSize f r''
                   (SendFile _ (Just (s,e))) -> return $!
                                                setContentLength (e-s) r''

            case rspContentLength z of
              Nothing   -> noCL z
              (Just sz) -> hasCL sz z

        -- HEAD requests cannot have bodies per RFC 2616 sec. 9.4
        if rqMethod req == HEAD
          then return $! deleteHeader "Transfer-Encoding" $
                         r''' { rspBody = Enum $ enumBuilder mempty }
          else return $! r'''


    --------------------------------------------------------------------------
    mkHeaderBuilder :: Response -> (Builder,Int)
    mkHeaderBuilder r = {-# SCC "mkHeaderBuilder" #-}
        ( mconcat [ fromByteString "HTTP/"
                  , fromString majstr
                  , fromWord8 $ c2w '.'
                  , fromString minstr
                  , space
                  , fromString $ statstr
                  , space
                  , fromByteString reason
                  , crlf
                  , hdrs
                  , crlf
                  ]
        , 12 + majlen + minlen + statlen + S.length reason + hlen )

      where
        (hdrs,hlen) = buildHdrs $ headers r
        majstr      = show major
        minstr      = show minor
        majlen      = length majstr
        minlen      = length minstr
        statstr     = show $ rspStatus r
        statlen     = length statstr
        crlf        = fromByteString "\r\n"
        space       = fromWord8 $ c2w ' '
        reason      = rspStatusReason r


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
    l  = liftM (map tl) $ H.lookup "Connection" hdrs
    tl = S.map (c2w . toLower . w2c)


------------------------------------------------------------------------------
-- FIXME: whitespace-trim the values here.
toHeaders :: [(ByteString,ByteString)] -> Headers
toHeaders kvps = H.fromList kvps'
  where
    kvps'      = map (first CI.mk) kvps


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
    fmt     = fromStr . formatTime defaultTimeLocale
                                   "%a, %d-%b-%Y %H:%M:%S GMT"


------------------------------------------------------------------------------
getFileSize :: FilePath -> IO FileOffset
getFileSize fp = liftM fileSize $ getFileStatus fp


------------------------------------------------------------------------------
l2s :: L.ByteString -> S.ByteString
l2s = S.concat . L.toChunks


------------------------------------------------------------------------------
toBS :: String -> ByteString
toBS = S.pack . map c2w
