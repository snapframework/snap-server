{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.Parser.Tests (tests) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad                        (liftM)
import           Control.Parallel.Strategies          (rdeepseq, using)
import qualified Data.ByteString.Char8                as S
import qualified Data.ByteString.Lazy.Char8           as L
import           Data.Int                             (Int64)
import qualified Data.Map                             as Map
import           Data.Monoid                          (mconcat)
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
import           Snap.Test.Common                     (coverEqInstance,
                                                       coverShowInstance,
                                                       coverTypeableInstance,
                                                       expectException)
import qualified System.IO.Streams                    as Streams


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testShow
        , testCookie
        , testChunked
        , testChunkDoS
        , testNull
        , testPartial
        , testParseError
        , testFormEncoded
        , testTrivials
        , testMethods
        , testSimpleParse
        , testSimpleParseErrors
        , testWriteChunkedTransferEncoding
        ]


------------------------------------------------------------------------------
testShow :: Test
testShow = testCase "parser/show" $ do
    let i = IRequest GET "/" (1, 1) Nothing []
    let !b = show i `using` rdeepseq
    return $ b `seq` ()


------------------------------------------------------------------------------
testNull :: Test
testNull = testCase "parser/shortParse" $
           expectException (Streams.fromList [] >>= parseRequest)


------------------------------------------------------------------------------
testPartial :: Test
testPartial = testCase "parser/partial" $
    expectException (Streams.fromList ["GET / "] >>= parseRequest)


------------------------------------------------------------------------------
testParseError :: Test
testParseError = testCase "parser/error" $ do
    expectException (Streams.fromList ["ZZZZZZZZZ"] >>= parseRequest)
    expectException (Streams.fromList ["GET / HTTP/1.1"] >>= parseRequest)
    expectException (Streams.fromList ["GET / HTTP/x.z\r\n\r\n"] >>=
                     parseRequest)


------------------------------------------------------------------------------
-- | convert a bytestring to chunked transfer encoding
transferEncodingChunked :: L.ByteString -> L.ByteString
transferEncodingChunked = f . L.toChunks
  where
    toChunk s = L.concat [ len, "\r\n", L.fromChunks [s], "\r\n" ]
      where
        len = L.pack $ printf "%x" $ S.length s

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
        chunked = transferEncodingChunked s


------------------------------------------------------------------------------
testWriteChunkedTransferEncoding :: Test
testWriteChunkedTransferEncoding = testCase "parser/writeChunked" $ do
    (os, getList) <- Streams.listOutputStream
    os' <- writeChunkedTransferEncoding os
    Streams.fromList [fromByteString "ok"] >>= Streams.connectTo os'
    Streams.write Nothing os'
    s <- liftM (toByteString . mconcat) getList
    assertEqual "chunked" "02\r\nok\r\n0\r\n\r\n" s


------------------------------------------------------------------------------
-- | ensure that running the 'readChunkedTransferEncoding' iteratee against
-- 'transferEncodingChunked' returns the original string
testChunkDoS :: Test
testChunkDoS = testCase "parser/chunkedTransferEncoding/DoS" $ do
    let n = ((2::Int64)^(18 :: Int64) + 10) :: Int64
    let s = S.concat $ L.toChunks $ L.take n $ L.fromChunks $
            cycle ["OKOKOKOKOKOKOKOK"]
    let ch = transferEncodingChunked $ L.fromChunks [s]

    expectException (Streams.fromList (L.toChunks ch) >>=
                     readChunkedTransferEncoding >>=
                     Streams.toList)


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
testFormEncoded :: Test
testFormEncoded = testCase "parser/formEncoded" $ do
    let bs = "foo1=bar1&foo2=bar2+baz2;foo3=foo%20bar"
    let mp = parseUrlEncoded bs

    assertEqual "foo1" (Just ["bar1"]     ) $ Map.lookup "foo1" mp
    assertEqual "foo2" (Just ["bar2 baz2"]) $ Map.lookup "foo2" mp
    assertEqual "foo3" (Just ["foo bar"]  ) $ Map.lookup "foo3" mp


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "parser/trivials" $ do
    coverTypeableInstance (undefined :: HttpParseException)
    coverShowInstance (HttpParseException "ok")
    coverEqInstance (IRequest GET "" (0, 0) Nothing [])


------------------------------------------------------------------------------
testMethods :: Test
testMethods = testCase "parser/methods" $ mapM_ testOne ms
  where
    ms = [ GET, POST, HEAD, PUT, DELETE, TRACE, OPTIONS, CONNECT, PATCH
         , Method "ZZZ" ]

    mToStr (Method m) = m
    mToStr m          = S.pack $ show m

    restOfRequest = [ " / HTTP/1.1\r\nz:b\r\nq:\r\nw\r\n", "foo: ", "bar"
                    , "\r\n  baz\r\n\r\n" ]

    testOne m = let s = mToStr m
                in Streams.fromList (s:restOfRequest) >>=
                   parseRequest >>=
                   checkMethod m

    checkMethod m i = do
        assertEqual "method" m $ iMethod i
        assertEqual "hdrs" [ ("z", "b")
                           , ("q", "")
                           , ("w", "")
                           , ("foo", "bar baz")
                           ] $ iRequestHeaders i


------------------------------------------------------------------------------
testSimpleParse :: Test
testSimpleParse = testCase "parser/simpleParse" $ do
    Streams.fromList ["GET / HTTP/1.1\r\n\r\n"] >>=
        parseRequest >>=
        assertEqual "simple0" (IRequest GET "/" (1, 1) Nothing [])

    Streams.fromList ["GET http://foo.com/ HTTP/1.1\r\n\r\n"] >>=
        parseRequest >>=
        assertEqual "simple1" (IRequest GET "/" (1, 1)
                                       (Just "foo.com") [])

    Streams.fromList ["GET http://foo.com HTTP/1.1\r\n\r\n"] >>=
        parseRequest >>=
        assertEqual "simple2" (IRequest GET "/" (1, 1)
                                           (Just "foo.com") [])

    Streams.fromList ["GET https://foo.com/ HTTP/1.1\r\n\r\n"] >>=
        parseRequest >>=
        assertEqual "simple3" (IRequest GET "/" (1, 1)
                                       (Just "foo.com") [])

    Streams.fromList ["GET / HTTP/1.1\r\nz:b\r\n", "", "\r\n"] >>=
        parseRequest >>=
        assertEqual "simple4" (IRequest GET "/" (1, 1) Nothing
                                        [("z", "b")])

    Streams.fromList [ "GET / HTTP/1.1\r\na:a\r", "\nz:b\r\n", ""
                     , "\r\n" ] >>=
        parseRequest >>=
        assertEqual "simple5" (IRequest GET "/" (1, 1) Nothing
                                        [("a", "a"), ("z", "b")])

    Streams.fromList ["GET /\r\n\r\n"] >>=
        parseRequest >>=
        assertEqual "simple6" (IRequest GET "/" (1, 0) Nothing [])

    Streams.fromList ["G", "ET", " /\r", "\n\r", "", "\n"] >>=
        parseRequest >>=
        assertEqual "simple7" (IRequest GET "/" (1, 0) Nothing [])


------------------------------------------------------------------------------
testSimpleParseErrors :: Test
testSimpleParseErrors = testCase "parser/simpleParseErrors" $ do
    expectException (
        Streams.fromList ["\r\nGET / HTTP/1.1\r\nz:b\r\n  \r\n"] >>=
        parseRequest)

    expectException (
        Streams.fromList ["\r\nGET / HTTP/1.1\r\nz:b\r "] >>=
        parseRequest)

    expectException (
        Streams.fromList ["\r\nGET / HTTP/1.1\r"] >>=
        parseRequest)

    expectException (
        Streams.fromList ["\r\nGET / HTTP/1.1\r", "", "foo\nz:b\r "] >>=
        parseRequest)
