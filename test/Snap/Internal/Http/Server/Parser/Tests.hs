{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.Parser.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Parallel.Strategies          (rdeepseq, using)
import qualified Data.ByteString                      as S
import           Data.ByteString.Internal             (c2w)
import qualified Data.ByteString.Lazy                 as L
import qualified Data.Map                             as Map
import           Data.Maybe                           (isNothing)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test, path)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic              hiding (assert, run)
import qualified Test.QuickCheck.Monadic              as QC
import           Text.Printf
------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Snap.Internal.Http.Server.Parser
import           Snap.Internal.Http.Types
import           Snap.Test.Common                     (expectException)
import qualified System.IO.Streams                    as Streams


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testShow
        , testCookie
        , testChunked
        , testNull
        , testPartial
        , testParseError
        , testFormEncoded ]


------------------------------------------------------------------------------
testShow :: Test
testShow = testCase "parser/show" $ do
    let i = IRequest GET "/" 1 1 []
    let !b = show i `using` rdeepseq
    return $ b `seq` ()


------------------------------------------------------------------------------
testNull :: Test
testNull = testCase "parser/shortParse" $ do
    f <- Streams.fromList [] >>= parseRequest
    assertBool "should be Nothing" $ isNothing f


------------------------------------------------------------------------------
testPartial :: Test
testPartial = testCase "parser/partial" $
    expectException (Streams.fromList ["GET / "] >>= parseRequest)


------------------------------------------------------------------------------
testParseError :: Test
testParseError = testCase "parser/error" $ do
    expectException (Streams.fromList ["ZZZZZZZZZ"] >>= parseRequest)


------------------------------------------------------------------------------
-- | convert a bytestring to chunked transfer encoding
transferEncodingChunked :: L.ByteString -> L.ByteString
transferEncodingChunked = f . L.toChunks
  where
    toChunk s = L.concat [ len, "\r\n", L.fromChunks [s], "\r\n" ]
      where
        len = L.pack $ map c2w $ printf "%x" $ S.length s

    f l = L.concat $ (map toChunk l ++ ["0\r\n\r\n"])


------------------------------------------------------------------------------
-- | ensure that running the 'readChunkedTransferEncoding' iteratee against
-- 'transferEncodingChunked' returns the original string
testChunked :: Test
testChunked = testProperty "parser/chunkedTransferEncoding" $
              monadicIO $ forAllM arbitrary prop_chunked
  where
    prop_chunked s = QC.run $ do
        debug "=============================="
        debug $ "input is " ++ show s
        debug $ "chunked is " ++ show chunked
        debug "------------------------------"

        out <- Streams.fromList (L.toChunks chunked) >>=
               readChunkedTransferEncoding >>=
               Streams.toList >>=
               return . L.fromChunks

        assertEqual "chunked" s out
        debug "==============================\n"

      where
        chunked = (transferEncodingChunked s)


testCookie :: Test
testCookie =
    testCase "parser/parseCookie" $ do
        assertEqual "cookie parsing" (Just [cv]) cv2

  where
    cv  = Cookie nm v Nothing Nothing Nothing False False
    cv2 = parseCookie ct

    nm     = "foo"
    v      = "bar"

    ct = S.concat [ nm , "=" , v ]


testFormEncoded :: Test
testFormEncoded = testCase "parser/formEncoded" $ do
    let bs = "foo1=bar1&foo2=bar2+baz2;foo3=foo%20bar"
    let mp = parseUrlEncoded bs

    assertEqual "foo1" (Just ["bar1"]     ) $ Map.lookup "foo1" mp
    assertEqual "foo2" (Just ["bar2 baz2"]) $ Map.lookup "foo2" mp
    assertEqual "foo3" (Just ["foo bar"]  ) $ Map.lookup "foo3" mp
