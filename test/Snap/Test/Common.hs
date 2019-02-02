{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

------------------------------------------------------------------------------
module Snap.Test.Common where

------------------------------------------------------------------------------
import           Control.DeepSeq             (deepseq)
import           Control.Exception.Lifted    (SomeException (..), catch, evaluate, finally, try)
import           Control.Monad               (liftM, replicateM)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.ByteString.Builder     (byteString, toLazyByteString)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as S
import qualified Data.ByteString.Lazy        as L
import           Data.Monoid                 (Monoid (mappend, mempty))
import           Data.Typeable               (Typeable, typeOf)
import           Network.Socket              (Socket)
import qualified Network.Socket              as N hiding (recv)
import           System.Timeout              (timeout)
import           Test.HUnit                  (assertFailure)
import           Test.QuickCheck             (Arbitrary (arbitrary), choose)

import qualified Network.Socket.ByteString   as N
#if !(MIN_VERSION_base(4,6,0))
import           Prelude                     hiding (catch)
#endif
------------------------------------------------------------------------------
instance Arbitrary S.ByteString where
    arbitrary = liftM S.pack arbitrary

instance Arbitrary L.ByteString where
    arbitrary = do
        n      <- choose(0,5)
        chunks <- replicateM n arbitrary
        return $! L.fromChunks chunks


------------------------------------------------------------------------------
expectException :: IO a -> IO ()
expectException m = do
    e <- try m
    case e of
      Left (z::SomeException) -> length (show z) `seq` return ()
      Right _ -> assertFailure "expected exception, didn't get it"


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
withSock :: Int -> (Socket -> IO a) -> IO a
withSock port go = do
    addr <- liftM (N.addrAddress . Prelude.head) $
            N.getAddrInfo (Just myHints)
                          (Just "127.0.0.1")
                          (Just $ show port)

    sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
    N.connect sock addr

    go sock `finally` close sock

  where
#if MIN_VERSION_network(2,7,0)
    close = N.close
#else
    close = N.sClose
#endif
    myHints = N.defaultHints { N.addrFlags = [ N.AI_NUMERICHOST ] }


------------------------------------------------------------------------------
recvAll :: Socket -> IO ByteString
recvAll sock = do
    b <- f mempty sock
    return $! S.concat $ L.toChunks $ toLazyByteString b

  where
    f b sk = do
        s <- N.recv sk 100000
        if S.null s
          then return b
          else f (b `mappend` byteString s) sk


------------------------------------------------------------------------------
ditchHeaders :: [ByteString] -> [ByteString]
ditchHeaders ("":xs)   = xs
ditchHeaders ("\r":xs) = xs
ditchHeaders (_:xs)    = ditchHeaders xs
ditchHeaders []        = []


------------------------------------------------------------------------------
forceSameType :: a -> a -> a
forceSameType _ a = a


------------------------------------------------------------------------------
-- | Kill the false negative on derived show instances.
coverShowInstance :: (Monad m, Show a) => a -> m ()
coverShowInstance x = a `deepseq` b `deepseq` c `deepseq` return ()
  where
    a = showsPrec 0 x ""
    b = show x
    c = showList [x] ""


------------------------------------------------------------------------------
coverReadInstance :: (MonadIO m, Read a) => a -> m ()
coverReadInstance x = do
    liftIO $ eatException $ evaluate $ forceSameType [(x,"")] $ readsPrec 0 ""
    liftIO $ eatException $ evaluate $ forceSameType [([x],"")] $ readList ""


------------------------------------------------------------------------------
coverEqInstance :: (Monad m, Eq a) => a -> m ()
coverEqInstance x = a `seq` b `seq` return ()
  where
    a = x == x
    b = x /= x


------------------------------------------------------------------------------
coverOrdInstance :: (Monad m, Ord a) => a -> m ()
coverOrdInstance x = a `deepseq` b `deepseq` return ()
  where
    a = [ x < x
        , x >= x
        , x > x
        , x <= x
        , compare x x == EQ ]

    b = min a $ max a a


------------------------------------------------------------------------------
coverTypeableInstance :: (Monad m, Typeable a) => a -> m ()
coverTypeableInstance a = typeOf a `seq` return ()


------------------------------------------------------------------------------
eatException :: (MonadBaseControl IO m) => m a -> m ()
eatException a = (a >> return ()) `catch` handler
  where
    handler :: (MonadBaseControl IO m) => SomeException -> m ()
    handler _ = return ()


------------------------------------------------------------------------------
timeoutIn :: Int -> IO a -> IO a
timeoutIn n m = timeout (n * 1000000) m >>= maybe (fail "timeout") return
