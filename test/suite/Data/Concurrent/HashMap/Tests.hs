{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Data.Concurrent.HashMap.Tests
  ( tests ) where

import           Data.ByteString.Char8 (ByteString)
import           Data.List
import           Data.Word
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import qualified Data.Concurrent.HashMap as H
import           Snap.Test.Common ()

tests :: [Test]
tests = [ testFromTo
        , testLookup
        , testDeletes
        , testUpdate
        , testNull
        ]


-- make sure we generate two strings which hash to the same bucket.
bogoHash :: ByteString -> Word
bogoHash "qqq" = 12345
bogoHash "zzz" = 12345
bogoHash x = H.hashBS x

testNull :: Test
testNull = testProperty "HashMap/null" $
           monadicIO $ forAllM arbitrary prop
  where
    prop :: [(Int,Int)] -> PropertyM IO ()
    prop l = do
        pre $ not $ null l
        ht <- run $ H.new H.hashInt
        b  <- run $ H.null ht
        assert b

        run $ mapM_ (\(k,v) -> H.insert k v ht) l
        b' <- run $ H.null ht
        assert $ not b'


testFromTo :: Test
testFromTo = testProperty "HashMap/fromList/toList" $
             monadicIO $ forAllM arbitrary prop
  where
    prop :: [(Int,Int)] -> PropertyM IO ()
    prop l = do
        ht <- run $ H.fromList H.hashInt l
        l' <- run $ H.toList ht

        let s1 = sort l
        let s2 = sort l'

        assert $ s1 == s2


testDeletes :: Test
testDeletes = testProperty "HashMap/deletes" $
              monadicIO $ forAllM arbitrary prop
  where
    prop :: [(ByteString,ByteString)] -> PropertyM IO ()
    prop l' = do
        pre (not $ null l')
        let l = [("qqq","QQQ"),("zzz","ZZZ")] ++ l'
        let h  = head l'

        ht <- run $ H.fromList bogoHash l
        v1 <- run $ H.lookup "qqq" ht
        v2 <- run $ H.lookup "zzz" ht

        run $ H.delete "qqq" ht
        v3 <- run $ H.lookup "qqq" ht
        v4 <- run $ H.lookup "zzz" ht

        run $ H.delete (fst h) ht
        run $ H.delete (fst h) ht

        v5 <- run $ H.lookup (fst h) ht

        assert $ v1 == Just "QQQ"
        assert $ v2 == Just "ZZZ"
        assert $ v3 == Nothing
        assert $ v4 == Just "ZZZ"
        assert $ v5 == Nothing


testLookup :: Test
testLookup = testProperty "HashMap/lookup" $
             monadicIO $ forAllM arbitrary prop
  where
    prop :: [(ByteString,ByteString)] -> PropertyM IO ()
    prop l' = do
        pre (not $ null l')
        let h  = head l'
        let l  = filter ((/= (fst h)) . fst) $ tail l'

        ht <- run $ H.fromList H.hashBS (h:l)

        v1 <- run $ H.lookup (fst h) ht
        run $ H.delete (fst h) ht
        v2 <- run $ H.lookup (fst h) ht

        assert $ v1 == (Just $ snd h)
        assert $ v2 == Nothing


testUpdate :: Test
testUpdate = testProperty "HashMap/update" $
             monadicIO $ forAllM arbitrary prop
  where
    prop :: [(ByteString,ByteString)] -> PropertyM IO ()
    prop l' = do
        pre (not $ null l')
        let h  = head l'
        let l  = filter ((/= (fst h)) . fst) $ tail l'

        ht <- run $ H.fromList H.hashBS (h:l)
        e1 <- run $ H.update (fst h) "qqq" ht
        v1 <- run $ H.lookup (fst h) ht
        run $ H.delete (fst h) ht
        v2 <- run $ H.lookup (fst h) ht
        e2 <- run $ H.update (fst h) "zzz" ht
        v3 <- run $ H.lookup (fst h) ht

        assert e1
        assert $ v1 == Just "qqq"
        assert $ v2 == Nothing
        assert $ not e2
        assert $ v3 == Just "zzz"
