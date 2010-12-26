{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module Snap.Test.Common where

import           Blaze.ByteString.Builder
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Internal (c2w)
import           Data.Monoid
import           Network.Socket
import qualified Network.Socket.ByteString as N
import           Prelude hiding (catch)
import           Test.QuickCheck
import           System.Timeout

import           Snap.Internal.Iteratee.Debug ()


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
                   

withSock :: Int -> (Socket -> IO a) -> IO a
withSock port go = do
    addr <- liftM (addrAddress . Prelude.head) $
            getAddrInfo (Just myHints)
                        (Just "127.0.0.1")
                        (Just $ show port)

    sock <- socket AF_INET Stream defaultProtocol
    connect sock addr

    go sock `finally` sClose sock

  where
    myHints = defaultHints { addrFlags = [ AI_NUMERICHOST ] }


recvAll :: Socket -> IO ByteString
recvAll sock = do
    b <- f mempty sock
    return $ toByteString b

  where
    f b sk = do
        s <- N.recv sk 100000
        if S.null s
          then return b
          else f (b `mappend` fromByteString s) sk


ditchHeaders :: [ByteString] -> [ByteString]
ditchHeaders ("":xs)   = xs
ditchHeaders ("\r":xs) = xs
ditchHeaders (_:xs)    = ditchHeaders xs
ditchHeaders []        = []

