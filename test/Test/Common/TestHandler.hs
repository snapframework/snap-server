{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Common.TestHandler (testHandler) where

------------------------------------------------------------------------------
import           Control.Concurrent         (threadDelay)
import           Control.Exception          (throwIO)
import           Control.Monad              (liftM)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (sort)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Monoid (mappend, mconcat, mempty))
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder   (Builder, flush, fromByteString)
------------------------------------------------------------------------------
import           Snap.Core                  (Request (rqParams, rqURI), Snap, getParam, getRequest, logError, modifyResponse, redirect, route, rqClientAddr, rqClientPort, setContentLength, setContentType, setHeader, setResponseBody, setResponseCode, setTimeout, transformRequestBody, writeBS, writeBuilder, writeLBS)
import           Snap.Internal.Debug        ()
import           Snap.Util.FileServe        (serveDirectory)
import           Snap.Util.FileUploads      (PartInfo (partContentType, partFileName), allowWithMaximumSize, defaultUploadPolicy, disallow, handleFileUploads)
import           Snap.Util.GZip             (noCompression, withCompression)
import           System.Directory           (createDirectoryIfMissing)
import           System.IO.Streams          (OutputStream)
import qualified System.IO.Streams          as Streams
import           Test.Common.Rot13          (rot13)


------------------------------------------------------------------------------
-- timeout handling
------------------------------------------------------------------------------
timeoutTickleHandler :: Snap ()
timeoutTickleHandler = do
    noCompression   -- FIXME: remove this when zlib-bindings and
                    -- zlib-enumerator support gzip stream flushing
    modifyResponse $ setResponseBody (trickleOutput 10)
                   . setContentType "text/plain"
    setTimeout 2


badTimeoutTickleHandler :: Snap ()
badTimeoutTickleHandler = do
    noCompression   -- FIXME: remove this when zlib-bindings and
                    -- zlib-enumerator support gzip stream flushing
    modifyResponse $ setResponseBody (trickleOutput 10)
                   . setContentType "text/plain"
    setTimeout 2


trickleOutput :: Int -> OutputStream Builder -> IO (OutputStream Builder)
trickleOutput n os = do
    Streams.fromList dots >>= Streams.mapM f >>= Streams.supplyTo os
    return os
  where
    dots = replicate n ".\n"
    f x  = threadDelay 1000000 >> return (fromByteString x `mappend` flush)


------------------------------------------------------------------------------
pongHandler :: Snap ()
pongHandler = modifyResponse $
              setResponseBody body .
              setContentType "text/plain" .
              setContentLength 4
  where
    body os = do Streams.write (Just $ fromByteString "PONG") os
                 return os

echoUriHandler :: Snap ()
echoUriHandler = do
    req <- getRequest
    writeBS $ rqURI req

echoHandler :: Snap ()
echoHandler = transformRequestBody return

rot13Handler :: Snap ()
rot13Handler = transformRequestBody (Streams.map rot13)

bigResponseHandler :: Snap ()
bigResponseHandler = do
    let sz = 4000000
    let s = L.take sz $ L.cycle $ L.fromChunks [S.replicate 400000 '.']
    modifyResponse $ setContentLength $ fromIntegral sz
    writeLBS s


responseHandler :: Snap ()
responseHandler = do
    !code <- liftM (read . S.unpack . fromMaybe "503") $ getParam "code"
    modifyResponse $ setResponseCode code
    writeBS $ S.pack $ show code


uploadForm :: Snap ()
uploadForm = do
    modifyResponse $ setContentType "text/html"
    writeBS form

  where
    form = S.concat [ "<html><head><title>Upload a file</title></head><body>\n"
                    , "<p>Upload some <code>text/plain</code> files:</p>\n"
                    , "<form method='post' "
                    , "enctype='multipart/form-data' "
                    , "action='/upload/handle'>\n"
                    , "<input type='file' "
                    , "accept='text/plain' "
                    , "multiple='true' "
                    , "name='file'></input>\n"
                    , "<input type='submit' name='Submit'></input>\n"
                    , "</form></body></html>" ]


uploadHandler :: Snap ()
uploadHandler = do
    logError "uploadHandler"
    liftIO $ createDirectoryIfMissing True tmpdir
    files <- handleFileUploads tmpdir defaultUploadPolicy partPolicy hndl
    let m = sort files

    params <- liftM (Prelude.map (\(a,b) -> (a,S.concat b)) .
                     Map.toAscList .
                     rqParams) getRequest

    modifyResponse $ setContentType "text/plain"
    writeBuilder $ buildRqParams params `mappend` buildFiles m

  where
    f p = fromMaybe "-" $ partFileName p

    hndl _ (Left e)          = throwIO e
    hndl partInfo (Right fp) = do
        !c <- liftIO $ S.readFile fp
        return $! (f partInfo, c)

    builder _ [] = mempty
    builder ty ((k,v):xs) =
        mconcat [ fromByteString ty
                , fromByteString ":\n"
                , fromByteString k
                , fromByteString "\nValue:\n"
                , fromByteString v
                , fromByteString "\n\n"
                , builder ty xs ]


    buildRqParams = builder "Param"
    buildFiles = builder "File"

    tmpdir = "dist/filetmp"
    partPolicy partInfo = if partContentType partInfo == "text/plain"
                            then allowWithMaximumSize 200000
                            else disallow

serverHeaderHandler :: Snap ()
serverHeaderHandler = modifyResponse $ setHeader "Server" "foo"


chunkedResponse :: Snap ()
chunkedResponse = writeBS "chunked"


remoteAddrPort :: Snap ()
remoteAddrPort = do
    rq <- getRequest
    let addr = rqClientAddr rq
    let port = rqClientPort rq
    let out = S.concat [ addr, ":", S.pack (show port) ]
    modifyResponse $ setContentLength $ fromIntegral $ S.length out
    writeBS out


testHandler :: Snap ()
testHandler = withCompression $
    route [ ("pong"              , pongHandler                       )
          , ("redirect"          , redirect "/pong"                  )
          , ("echo"              , echoHandler                       )
          , ("rot13"             , rot13Handler                      )
          , ("echoUri"           , echoUriHandler                    )
          , ("remoteAddrPort"    , remoteAddrPort                    )
          , ("fileserve"         , noCompression >>
                                   serveDirectory "testserver/static")
          , ("bigresponse"       , bigResponseHandler                )
          , ("respcode/:code"    , responseHandler                   )
          , ("upload/form"       , uploadForm                        )
          , ("upload/handle"     , uploadHandler                     )
          , ("timeout/tickle"    , timeoutTickleHandler              )
          , ("timeout/badtickle" , badTimeoutTickleHandler           )
          , ("server-header"     , serverHeaderHandler               )
          , ("chunked"           , chunkedResponse                   )
          ]
