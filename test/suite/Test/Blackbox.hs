{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Test.Blackbox
  ( tests
  , startTestServer ) where


import             Control.Concurrent
import             Control.Exception (try, SomeException)
import             Control.Monad
import "monads-fd" Control.Monad.Trans
import qualified   Data.ByteString as S
import qualified   Data.ByteString.Lazy as L
import qualified   Data.ByteString.Lazy.Char8 as LC
import             Data.ByteString (ByteString)
import             Data.ByteString.Internal (c2w, w2c)
import             Data.Char
import             Data.Int
import             Data.IORef
import             Data.Iteratee.WrappedByteString
import qualified   Data.Map as Map
import             Data.Maybe (fromJust)
import             Data.Monoid
import             Data.Time.Calendar
import             Data.Time.Clock
import             Data.Word
import qualified   Network.URI as URI
import qualified   Network.HTTP as HTTP
import             Prelude hiding (take)
import qualified   Prelude
import             Test.Framework
import             Test.Framework.Providers.HUnit
import             Test.Framework.Providers.QuickCheck2
import             Test.HUnit hiding (Test, path)
import             Test.QuickCheck
import qualified   Test.QuickCheck.Monadic as QC
import             Test.QuickCheck.Monadic hiding (run, assert)

import             Snap.Http.Server
import             Snap.Iteratee
import             Snap.Test.Common ()
import             Snap.Types

import             Test.Common.Rot13
import             Test.Common.TestHandler


tests :: Int -> [Test]
tests port = [ testPong  port
             , testEcho  port
             , testRot13 port ]


startTestServer :: IO (ThreadId,Int)
startTestServer = do
    tid <- forkIO $
           httpServe "*"
                     port
                     "localhost"
                     (Just "ts-access.log")
                     (Just "ts-error.log")
                     testHandler
    waitabit

    return $ (tid, port)

  where
    port = 8199


testPong :: Int -> Test
testPong port = testCase "blackbox/pong" $ do
    rsp <- HTTP.simpleHTTP $
           HTTP.getRequest $
           "http://localhost:" ++ show port ++ "/pong"

    doc <- HTTP.getResponseBody rsp
    assertEqual "pong response" "PONG" doc


testEcho :: Int -> Test
testEcho port = testProperty "blackbox/echo" $
                monadicIO $ forAllM arbitrary prop
  where
    prop txt = do
        let uri = fromJust $
                  URI.parseURI $
                  "http://localhost:" ++ show port ++ "/echo"

        let len = S.length txt

        let req' = (HTTP.mkRequest HTTP.POST uri) :: HTTP.Request S.ByteString
        let req = HTTP.replaceHeader HTTP.HdrContentLength (show len) req'
                  
        rsp <- QC.run $ HTTP.simpleHTTP $ req { HTTP.rqBody = (txt::S.ByteString) }
        doc <- QC.run $ HTTP.getResponseBody rsp

        QC.assert $ txt == doc


testRot13 :: Int -> Test
testRot13 port = testProperty "blackbox/rot13" $
                 monadicIO $ forAllM arbitrary prop
  where
    prop txt = do
        let uri = fromJust $
                  URI.parseURI $
                  "http://localhost:" ++ show port ++ "/rot13"

        let len = S.length txt

        let req' = (HTTP.mkRequest HTTP.POST uri) :: HTTP.Request S.ByteString
        let req = HTTP.replaceHeader HTTP.HdrContentLength (show len) req'
                  
        rsp <- QC.run $ HTTP.simpleHTTP $ req { HTTP.rqBody = (txt::S.ByteString) }
        doc <- QC.run $ HTTP.getResponseBody rsp

        QC.assert $ txt == rot13 doc


------------------------------------------------------------------------------
waitabit :: IO ()
waitabit = threadDelay $ 2*((10::Int)^(6::Int))
