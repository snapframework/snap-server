{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Parser.Tests
  ( tests ) where

import qualified Control.Exception as E
import           Control.Exception hiding (try, assert)
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Parallel.Strategies
import           Data.Attoparsec hiding (Result(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Internal (c2w)
import           Data.IORef
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe (isNothing)
import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic hiding (run, assert)
import           Test.HUnit hiding (Test, path)
import           Text.Printf

import           Snap.Internal.Http.Parser
import           Snap.Internal.Http.Types
import           Snap.Internal.Debug
import           Snap.Internal.Iteratee.Debug
import           Snap.Iteratee hiding (map, sequence)
import qualified Snap.Iteratee as I
import           Snap.Test.Common()


tests :: [Test]
tests = [ testShow
        , testCookie
        , testChunked
        , testBothChunked
        , testBothChunkedPipelined
        , testBothChunkedEmpty
        , testP2I
        , testNull
        , testPartial
        , testParseError
        , testFormEncoded ]


emptyParser :: Parser ByteString
emptyParser = option "foo" $ string "bar"

testShow :: Test
testShow = testCase "parser/show" $ do
    let i = IRequest GET "/" (1,1) []
    let !b = show i `using` rdeepseq
    return $ b `seq` ()


testP2I :: Test
testP2I = testCase "parser/iterParser" $ do
    i <- liftM (enumBS "z") $ runIteratee (iterParser emptyParser)
    l <- run_ i

    assertEqual "should be foo" "foo" l


forceErr :: SomeException -> IO ()
forceErr e = f `seq` (return ())
  where
    !f = show e


testNull :: Test
testNull = testCase "parser/shortParse" $ do
    f <- run_ (parseRequest)
    assertBool "should be Nothing" $ isNothing f


testPartial :: Test
testPartial = testCase "parser/partial" $ do
    i <- liftM (enumBS "GET / ") $ runIteratee parseRequest
    f <- E.try $ run_ i

    case f of (Left e)  -> forceErr e
              (Right x) -> assertFailure $ "expected exception, got " ++ show x


testParseError :: Test
testParseError = testCase "parser/error" $ do
    step <- runIteratee parseRequest
    let i = enumBS "ZZZZZZZZZZ" step
    f <- E.try $ run_ i

    case f of (Left e)  -> forceErr e
              (Right x) -> assertFailure $ "expected exception, got " ++ show x


-- | convert a bytestring to chunked transfer encoding
transferEncodingChunked :: L.ByteString -> L.ByteString
transferEncodingChunked = f . L.toChunks
  where
    toChunk s = L.concat [ len, "\r\n", L.fromChunks [s], "\r\n" ]
      where
        len = L.pack $ map c2w $ printf "%x" $ S.length s

    f l = L.concat $ (map toChunk l ++ ["0\r\n\r\n"])


-- | ensure that running the 'readChunkedTransferEncoding' iteratee against
-- 'transferEncodingChunked' returns the original string
testChunked :: Test
testChunked = testProperty "parser/chunkedTransferEncoding" $
              monadicIO $ forAllM arbitrary prop_chunked
  where
    prop_chunked s = do
        QC.run $ debug "=============================="
        QC.run $ debug $ "input is " ++ show s
        QC.run $ debug $ "chunked is " ++ show chunked
        QC.run $ debug "------------------------------"
        sstep <- QC.run $ runIteratee $ stream2stream
        step  <- QC.run $ runIteratee $ 
                 joinI $ readChunkedTransferEncoding sstep

        out   <- QC.run $ run_ $ enum step

        QC.assert $ s == out
        QC.run $ debug "==============================\n"

      where
        chunked = (transferEncodingChunked s)
        enum = enumLBS chunked


testBothChunked :: Test
testBothChunked = testProperty "parser/invertChunked" $
                  monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        sstep <- QC.run $ runIteratee stream2stream
        let it = joinI $ writeChunkedTransferEncoding sstep

        bs <- QC.run $ runIteratee it >>= run_ . enumLBS s

        let enum = enumLBS bs

        x <- QC.run $
             runIteratee (joinI $ readChunkedTransferEncoding sstep) >>=
             run_ . enum

        QC.assert $ s == x



testBothChunkedPipelined :: Test
testBothChunkedPipelined = testProperty "parser/testBothChunkedPipelined" $
                           monadicIO prop
  where
    prop = do
        sz     <- QC.pick (choose (1000,4000))
        s'     <- QC.pick $ resize sz arbitrary
        ntimes <- QC.pick (choose (4,7))
        --let s' = L.take 2000 $ L.fromChunks $ repeat s

        let e = enumLBS s'
        let n = fromEnum $ L.length s'

        let enum = foldl' (>==>) (enumBS "") (replicate ntimes e)

        bufi <- QC.run $
                unsafeBufferIteratee copyingStream2Stream >>= runIteratee

        iter' <- QC.run $ runIteratee $ joinI $
                 writeChunkedTransferEncoding bufi
        let iter = I.joinI $ I.take n iter'

        let iters = replicate ntimes iter
        let mothra = foldM (\s it -> it >>= \t -> return $ s `mappend` t)
                           mempty
                           iters

        bs <- QC.run $ runIteratee mothra >>= run_ . enum

        let e2 = enumBS bs

        let pcrlf = \s -> iterParser $ string "\r\n" >> return s

        sstep <- QC.run $ runIteratee stream2stream

        let iters = replicate ntimes $ joinI $
                    readChunkedTransferEncoding sstep
        let godzilla = sequence $ map (>>= pcrlf) iters

        x <- QC.run $ runIteratee godzilla >>= run_ . e2

        QC.assert $
          x == (replicate ntimes s')



testBothChunkedEmpty :: Test
testBothChunkedEmpty = testCase "parser/testBothChunkedEmpty" prop
  where
    prop = do
        let s' = ""
        let e = enumLBS s'
        let n = fromEnum $ L.length s'

        let ntimes = 5
        let enum = foldl' (>==>) (enumBS "") (replicate ntimes e)

        sstep <- runIteratee stream2stream

        step <- runIteratee $
                joinI $
                writeChunkedTransferEncoding sstep
        iter <- liftM returnI $ runIteratee $ joinI $ I.take n step

        let iters = replicate ntimes (iter :: Iteratee ByteString IO L.ByteString)
        let mothra = foldM (\s it -> it >>= \t -> return $ s `mappend` t)
                           mempty
                           iters

        mothraStep <- runIteratee mothra
        bs <- run_ $ enum mothraStep

        let e2 = enumLBS bs

        let pcrlf = \s -> iterParser $ string "\r\n" >> return s

        let iters = replicate ntimes $ joinI $
                    readChunkedTransferEncoding sstep
        godzilla <- runIteratee $ sequence $ map (>>= pcrlf) iters

        x <- run_ $ e2 godzilla

        assertBool "empty chunked transfer" $
          x == (replicate ntimes s')


testCookie :: Test
testCookie =
    testCase "parser/parseCookie" $ do
        assertEqual "cookie parsing" (Just [cv]) cv2

  where
    cv  = Cookie nm v Nothing Nothing Nothing
    cv2 = parseCookie ct

    nm     = "foo"
    v      = "bar"

    ct = S.concat [ nm
                  , "="
                  , v ]


testFormEncoded :: Test
testFormEncoded = testCase "parser/formEncoded" $ do
    let bs = "foo1=bar1&foo2=bar2+baz2&foo3=foo%20bar"
    let mp = parseUrlEncoded bs

    assertEqual "foo1" (Just ["bar1"]     ) $ Map.lookup "foo1" mp
    assertEqual "foo2" (Just ["bar2 baz2"]) $ Map.lookup "foo2" mp
    assertEqual "foo3" (Just ["foo bar"]  ) $ Map.lookup "foo3" mp




copyingStream2Stream = go []
  where
    go l = do
        mbx <- I.head
        maybe (return $ S.concat $ reverse l)
              (\x -> let !z = S.copy x in go (z:l))
              mbx

stream2stream :: (Monad m) => Iteratee ByteString m L.ByteString
stream2stream = liftM L.fromChunks consume
