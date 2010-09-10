{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module Snap.Test.Common where

import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Internal (c2w)
import           Prelude hiding (catch)
import           Test.QuickCheck
import           System.Timeout

import           Snap.Internal.Iteratee.Debug ()

import System.IO

instance Arbitrary S.ByteString where
    arbitrary = liftM (S.pack . map c2w) arbitrary

instance Arbitrary L.ByteString where
    arbitrary = do
        n      <- choose(0,5)
        chunks <- replicateM n arbitrary
        return $ L.fromChunks chunks



expectExceptionBeforeTimeout :: IO a    -- ^ action to run
                             -> Int     -- ^ number of seconds to expect
                                        -- exception by
                             -> IO Bool
expectExceptionBeforeTimeout act nsecs = do
    x <- timeout (nsecs * (10::Int)^(6::Int)) f
    case x of
      Nothing  -> return False
      (Just y) -> return y

  where
    f = (act >> return False) `catch` \(e::SomeException) -> do
            if show e == "<<timeout>>"
               then return False
               else return True
                   
