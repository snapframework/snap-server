{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Common.TestHandler (testHandler) where

import           Blaze.ByteString.Builder
import           Control.Concurrent         (threadDelay)
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Monoid
import           Snap.Core
import           Snap.Internal.Debug
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           Snap.Util.GZip
import           System.Directory
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
    dots = replicate n $ fromByteString ".\n"
    f x  = threadDelay 1000000 >> return (x `mappend` flush)


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


{-
echoHandler :: Snap ()
echoHandler = transformRequestBody returnI


rot13Handler :: Snap ()
rot13Handler = transformRequestBody f
  where
    f origStep = do
        mbX  <- I.head
        maybe (enumEOF origStep)
              (feedStep origStep)
              mbX

    feedStep origStep b = do
        let x = toByteString b
        let e = enumBuilder $ fromByteString $ rot13 x
        step <- lift $ runIteratee $ e origStep
        f step
-}

bigResponseHandler :: Snap ()
bigResponseHandler = do
    let sz = 4000000
    let s = L.take sz $ L.cycle $ L.fromChunks [S.replicate 400000 '.']
    modifyResponse $ setContentLength sz
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


{-
uploadHandler :: Snap ()
uploadHandler = do
    liftIO $ createDirectoryIfMissing True tmpdir
    handleFileUploads tmpdir defaultUploadPolicy partPolicy hndl

  where
    isRight (Left _) = False
    isRight (Right _) = True

    f (_, Left _) = error "impossible"
    f (p, Right x) = (fromMaybe "-" $ partFileName p, x)

    hndl xs' = do
        let xs = [ f x | x <- xs', isRight (snd x) ]

        files <- mapM (\(x,fp) -> do
                           c <- liftIO $ S.readFile fp
                           return (x,c)) xs

        let m = sort files

        params <- liftM (Prelude.map (\(a,b) -> (a,S.concat b)) .
                         Map.toAscList .
                         rqParams) getRequest

        modifyResponse $ setContentType "text/plain"
        writeBuilder $ buildRqParams params `mappend` buildFiles m


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
-}

serverHeaderHandler :: Snap ()
serverHeaderHandler = modifyResponse $ setHeader "Server" "foo"


testHandler :: Snap ()
testHandler = withCompression $
    route [ ("pong"              , pongHandler                       )
--          , ("echo"              , echoHandler                       )
--          , ("rot13"             , rot13Handler                      )
          , ("echoUri"           , echoUriHandler                    )
          , ("fileserve"         , serveDirectory "testserver/static")
          , ("bigresponse"       , bigResponseHandler                )
          , ("respcode/:code"    , responseHandler                   )
          , ("upload/form"       , uploadForm                        )
--          , ("upload/handle"     , uploadHandler                     )
          , ("timeout/tickle"    , timeoutTickleHandler              )
          , ("timeout/badtickle" , badTimeoutTickleHandler           )
          , ("server-header"     , serverHeaderHandler               )
          ]
