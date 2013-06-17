{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
module System.SendFile.Tests (tests) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder       (fromByteString)
import           Control.Concurrent.MVar
import           Control.Exception              (evaluate)
import           Control.Monad                  (void)
import qualified Data.ByteString.Char8          as S
import           Foreign.C.Error
import           Foreign.C.Types
import           Foreign.Ptr
import           System.Posix.Types
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test, path)
------------------------------------------------------------------------------
import           Snap.Test.Common               (expectException)
import qualified System.SendFile                as SF

tests :: [Test]
tests = [ testSendHeaders
        , testSendHeaderCrash
        , testTrivials
        ]


------------------------------------------------------------------------------
testSendHeaders :: Test
testSendHeaders = testCase "sendfile/sendHeaders" $ do
    callLog <- newMVar []
    sampleData <- newMVar sampleActions
    nWaits <- newMVar (0 :: Int)
    let bumpWaits = \x -> x `seq` modifyMVar_ nWaits (return . (+1))
    SF.sendHeadersImpl (sendHeadersMockSendFunc sampleData callLog) bumpWaits
                       builder 100
    [c1, c2, c3] <- readMVar callLog
    assertEqual "sendHeaders1" c1 c2
    assertEqual "sendHeaders2" 8 (_sz c3)
    readMVar nWaits >>= assertEqual "sendHeaders3" 1

  where
    builder       = fromByteString $ S.replicate 10 ' '
    sampleActions = [ c_set_errno eAGAIN >> return (-1)
                    , return 2
                    , return 8
                    ]


------------------------------------------------------------------------------
testSendHeaderCrash :: Test
testSendHeaderCrash = testCase "sendfile/sendHeaders/crash" $ do
    callLog <- newMVar []
    sampleData <- newMVar sampleActions
    nWaits <- newMVar (0 :: Int)
    let bumpWaits = \x -> x `seq` modifyMVar_ nWaits (return . (+1))
    expectException $
        SF.sendHeadersImpl (sendHeadersMockSendFunc sampleData callLog) bumpWaits
                           builder 100
  where
    builder       = fromByteString $ S.replicate 10 ' '
    sampleActions = [ c_set_errno eCONNRESET >> return (-1) ]

------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "sendfile/trivials" $
               void (evaluate $ length SF.sendFileMode)


------------------------------------------------------------------------------
data CallLog = CallLog { _fd    :: Fd
                       , _str   :: Ptr CChar
                       , _sz    :: CSize
                       , _flags :: CInt
                       }
  deriving (Eq, Show, Ord)

------------------------------------------------------------------------------
sendHeadersMockSendFunc :: MVar [IO CSize]             -- ^ sample outputs
                        -> MVar [CallLog]              -- ^ log of calls
                        -> Fd -> Ptr CChar -> CSize -> CInt -> IO CSize
sendHeadersMockSendFunc sampleData callLog fd cstr clen flags = do
    modifyMVar_ callLog (return . (++ [CallLog fd cstr clen flags]))
    x <- modifyMVar sampleData $ \xs -> return $!
            if null xs then ([], return Nothing) else (tail xs, fmap Just $! head xs)
    x >>= maybe (c_set_errno eCONNRESET >> return (-1))
                (return)


foreign import ccall unsafe "set_errno" c_set_errno :: Errno -> IO ()
