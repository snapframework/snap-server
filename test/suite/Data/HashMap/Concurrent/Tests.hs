{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Data.HashMap.Concurrent.Tests
  ( tests ) where

import           Data.List
import           Test.Framework 
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import qualified Data.HashMap.Concurrent as H

tests :: [Test]
tests = [ testFromTo ]


testFromTo :: Test
testFromTo = testProperty "HashMap/fromList/toList" $
             monadicIO $ forAllM arbitrary prop
  where
    prop l = do
        ht <- run $ H.fromList H.hashInt (l :: [(Int,Int)])
        l' <- run $ H.toList ht

        let s1 = sort l
        let s2 = sort l'

        assert $ s1 == s2
