{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Parser.Tests
  ( tests ) where

import qualified Control.Exception as E
import           Control.Exception hiding (try, assert)
import           Control.Monad
import           Control.Monad.Identity
import           Control.Parallel.Strategies
import           Data.Attoparsec hiding (Result(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Internal (c2w)
import           Data.IORef
import           Data.Iteratee.WrappedByteString
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
import           Snap.Internal.Http.Types hiding (Enumerator)
import           Snap.Iteratee hiding (foldl')
import qualified Snap.Iteratee as I
import           Snap.Test.Common()


tests :: [Test]
tests = [ testShow
        , testCookie
        , testChunked
        , testBothChunked
        , testBothChunkedBuffered1
        , testBothChunkedPipelined
        , testBothChunkedEmpty
        , testP2I
        , testNull
        , testPartial
        , testIterateeError
        , testIterateeError2
        , testParseError
        , testFormEncoded ]


emptyParser :: Parser ByteString
emptyParser = option "foo" $ string "bar"

testShow :: Test
testShow = testCase "show" $ do
    let i = IRequest GET "/" (1,1) []
    let !b = show i `using` rdeepseq
    return $ b `seq` ()


testP2I :: Test
testP2I = testCase "parserToIteratee" $ do
    i <- enumBS "z" (parserToIteratee emptyParser)
    l <- run i

    assertEqual "should be foo" "foo" l


forceErr :: SomeException -> IO ()
forceErr e = f `seq` (return ())
  where
    !f = show e


testNull :: Test
testNull = testCase "short parse" $ do
    f <- run (parseRequest)
    assertBool "should be Nothing" $ isNothing f


testPartial :: Test
testPartial = testCase "partial parse" $ do
    i <- enumBS "GET / " parseRequest
    f <- E.try $ run i

    case f of (Left e)  -> forceErr e
              (Right x) -> assertFailure $ "expected exception, got " ++ show x


testParseError :: Test
testParseError = testCase "parse error" $ do
    i <- enumBS "ZZZZZZZZZZ" parseRequest
    f <- E.try $ run i

    case f of (Left e)  -> forceErr e
              (Right x) -> assertFailure $ "expected exception, got " ++ show x


introduceError :: (Monad m) => Enumerator m a
introduceError iter = return $ IterateeG $ \_ ->
                          runIter iter (EOF (Just (Err "EOF")))

testIterateeError :: Test
testIterateeError = testCase "iteratee error" $ do
    i <- liftM liftI $ runIter parseRequest (EOF (Just (Err "foo")))
    f <- E.try $ run i

    case f of (Left e)  -> forceErr e
              (Right x) -> assertFailure $ "expected exception, got " ++ show x

testIterateeError2 :: Test
testIterateeError2 = testCase "iteratee error 2" $ do
    i <- (enumBS "GET / " >. introduceError) parseRequest
    f <- E.try $ run i

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
testChunked = testProperty "chunked transfer encoding" prop_chunked
  where
    prop_chunked :: L.ByteString -> Bool
    prop_chunked s = runIdentity (run iter) == s
      where
        enum = enumLBS (transferEncodingChunked s)

        iter :: Iteratee Identity L.ByteString
        iter = runIdentity $ do
                   i <- (readChunkedTransferEncoding stream2stream) >>= enum 
                   return $ liftM fromWrap i

testBothChunked :: Test
testBothChunked = testProperty "chunk . unchunk == id" $
                  monadicIO $ forAllM arbitrary prop
  where
    prop s = do
        it <- QC.run $ writeChunkedTransferEncoding stream2stream

        bs <- QC.run $
              enumBS s it
                >>= run >>= return . unWrap

        let enum = enumBS bs

        iter <- do
            i <- (readChunkedTransferEncoding stream2stream) >>= enum 
            return $ liftM unWrap i

        x <- run iter
        QC.assert $ s == x


testBothChunkedBuffered1 :: Test
testBothChunkedBuffered1 = testProperty "testBothChunkedBuffered2" $
                           monadicIO prop
  where
    prop = do
        sz     <- QC.pick (choose (1000,4000))
        s'     <- QC.pick $ resize sz arbitrary
        ntimes <- QC.pick (choose (4,7))

        let e = enumLBS s'
        let n = fromEnum $ L.length s'

        let enum = foldl' (>.) (enumBS "") (replicate ntimes e)

        (bufi,_) <- QC.run $ bufferIteratee stream2stream
        iter' <- QC.run $ writeChunkedTransferEncoding bufi
        let iter = I.joinI $ I.take n iter'
        let iters = replicate ntimes iter

        let mothra = foldM (\s it -> it >>= \t -> return $ s `mappend` t)
                           mempty
                           iters

        bs <- QC.run $ enum mothra
                >>= run >>= return . unWrap


        ----------------------------------------------------------------------
        -- 2nd pass, cancellation
        let pcrlf = \s -> parserToIteratee $ string "\r\n" >> return s
        (inputIter2,esc) <- QC.run $ bufferIteratee stream2stream
        QC.run $ writeIORef esc True

        iter2' <- QC.run $ writeChunkedTransferEncoding inputIter2
        let iter2 = I.joinI $ I.take n iter2'
        let iters2 = replicate ntimes iter2

        let mothra2 = foldM (\s it -> it >>= \t -> return $ s `mappend` t)
                            mempty
                            iters2


        bs2 <- QC.run $ enum mothra2
                 >>= run >>= return . unWrap


        let e2 = enumBS bs2
        iters' <- QC.run $
                  replicateM ntimes $
                    readChunkedTransferEncoding stream2stream
        let godzilla2 = sequence $ map (>>= pcrlf) iters'
        outiter2 <- QC.run $ e2 godzilla2
        x2 <- QC.run $ liftM (map unWrap) $ run outiter2

        QC.assert $
          (map (L.fromChunks . (:[])) x2) == (replicate ntimes s')



testBothChunkedPipelined :: Test
testBothChunkedPipelined = testProperty "testBothChunkedPipelined" $
                           monadicIO prop
  where
    prop = do
        sz     <- QC.pick (choose (1000,4000))
        s'     <- QC.pick $ resize sz arbitrary
        ntimes <- QC.pick (choose (4,7))
        --let s' = L.take 2000 $ L.fromChunks $ repeat s

        let e = enumLBS s'
        let n = fromEnum $ L.length s'

        let enum = foldl' (>.) (enumBS "") (replicate ntimes e)

        (bufi,_) <- QC.run $ bufferIteratee stream2stream

        iter' <- QC.run $ writeChunkedTransferEncoding bufi
        let iter = I.joinI $ I.take n iter'

        let iters = replicate ntimes iter
        let mothra = foldM (\s it -> it >>= \t -> return $ s `mappend` t)
                           mempty
                           iters

        bs <- QC.run $ enum mothra
                >>= run >>= return . unWrap

        let e2 = enumBS bs

        let pcrlf = \s -> parserToIteratee $ string "\r\n" >> return s

        iters <- QC.run $
                 replicateM ntimes $
                   readChunkedTransferEncoding stream2stream
        let godzilla = sequence $ map (>>= pcrlf) iters

        iter <- QC.run $ e2 godzilla

        x <- QC.run $ liftM (map unWrap) $ run iter

        QC.assert $
          (map (L.fromChunks . (:[])) x) == (replicate ntimes s')



testBothChunkedEmpty :: Test
testBothChunkedEmpty = testCase "testBothChunkedEmpty" prop
  where
    prop = do
        let s' = ""
        let e = enumLBS s'
        let n = fromEnum $ L.length s'

        let ntimes = 5
        let enum = foldl' (>.) (enumBS "") (replicate ntimes e)

        iter' <- writeChunkedTransferEncoding stream2stream
        let iter = I.joinI $ I.take n iter'

        let iters = replicate ntimes iter
        let mothra = foldM (\s it -> it >>= \t -> return $ s `mappend` t)
                           mempty
                           iters

        bs <- enum mothra
                >>= run >>= return . unWrap

        let e2 = enumBS bs

        let pcrlf = \s -> parserToIteratee $ string "\r\n" >> return s

        iters <- replicateM ntimes $
                   readChunkedTransferEncoding stream2stream
        let godzilla = sequence $ map (>>= pcrlf) iters

        iter <- e2 godzilla

        x <- liftM (map unWrap) $ run iter

        assertBool "empty chunked transfer" $
          (map (L.fromChunks . (:[])) x) == (replicate ntimes s')


testCookie :: Test
testCookie =
    testCase "parseCookie" $ do
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
testFormEncoded = testCase "formEncoded" $ do
    let bs = "foo1=bar1&foo2=bar2+baz2&foo3=foo%20bar"
    let mp = parseUrlEncoded bs

    assertEqual "foo1" (Just ["bar1"]     ) $ Map.lookup "foo1" mp
    assertEqual "foo2" (Just ["bar2 baz2"]) $ Map.lookup "foo2" mp
    assertEqual "foo3" (Just ["foo bar"]  ) $ Map.lookup "foo3" mp
