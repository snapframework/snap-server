{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
module System.SendFile.Tests (tests) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder       (fromByteString)
import           Control.Concurrent.MVar
import           Control.Exception              (evaluate)
import           Control.Monad
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

#if defined(LINUX)
import qualified System.SendFile.Linux          as SFI
#elif defined(FREEBSD)
import           Foreign.Storable
import qualified System.SendFile.FreeBSD        as SFI
#elif defined(OSX)
import           Foreign.Storable
import qualified System.SendFile.Darwin         as SFI
#endif

tests :: [Test]
tests = [ testSendHeaders
        , testSendHeaderCrash
        , testSendFile
        , testSendFileCrash
        , testSendFileZero
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
data SendHeadersCallLog = SendHeadersCallLog {
      _fd    :: Fd
    , _str   :: Ptr CChar
    , _sz    :: CSize
    , _flags :: CInt
    }
  deriving (Eq, Show, Ord)

------------------------------------------------------------------------------
sendHeadersMockSendFunc :: MVar [IO CSize]             -- ^ sample outputs
                        -> MVar [SendHeadersCallLog]   -- ^ log of calls
                        -> Fd -> Ptr CChar -> CSize -> CInt -> IO CSize
sendHeadersMockSendFunc sampleData callLog !fd !cstr !clen !flags = do
    modifyMVar_ callLog (return . (++ [SendHeadersCallLog fd cstr clen flags]))
    x <- modifyMVar sampleData $ \xs -> return $!
            if null xs then ([], return Nothing) else (tail xs, fmap Just $! head xs)
    x >>= maybe (c_set_errno eCONNRESET >> return (-1))
                (return)


foreign import ccall unsafe "set_errno" c_set_errno :: Errno -> IO ()


------------------------------------------------------------------------------
-- Testing for internal sendfile via dep injection
#if defined(LINUX)
data SendFileCallLog = SendFileCallLog {
      _sf_fd1 :: Fd
    , _sf_fd2 :: Fd
    , _sf_ptr :: Ptr COff
    , _sf_sz  :: CSize
    }
  deriving (Eq, Show, Ord)


------------------------------------------------------------------------------
sendFileMockSendFunc :: MVar [IO CSize]          -- ^ sample outputs
                     -> MVar [SendFileCallLog]   -- ^ log of calls
                     -> Fd -> Fd -> Ptr COff -> CSize -> IO CSsize
sendFileMockSendFunc sampleData callLog !fd1 !fd2 !cstr !clen = do
    modifyMVar_ callLog (return . (++ [SendFileCallLog fd1 fd2 cstr clen]))
    x <- modifyMVar sampleData $ \xs -> return $!
            if null xs then ([], return Nothing) else (tail xs, fmap Just $! head xs)
    x >>= maybe (c_set_errno eCONNRESET >> return (-1))
                (return . fromIntegral)

#elif defined(FREEBSD)
data SendFileCallLog = SendFileCallLog {
      _sf_fd1 :: Fd
    , _sf_fd2 :: Fd
    , _sf_off :: COff
    , _sf_sz  :: COff
    }
  deriving (Eq, Show, Ord)

------------------------------------------------------------------------------
sendFileMockSendFunc :: MVar [IO CSize]          -- ^ sample outputs
                     -> MVar [SendFileCallLog]   -- ^ log of calls
                     -> Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff
                     -> CInt -> IO CInt
sendFileMockSendFunc sampleData callLog !fd1 !fd2 !off !clen !_ !pbytes !_ = do
    modifyMVar_ callLog (return . (++ [SendFileCallLog fd1 fd2 off clen]))
    x <- modifyMVar sampleData $ \xs -> return $!
            if null xs then ([], return Nothing) else (tail xs, fmap Just $! head xs)
    x >>= maybe (c_set_errno eCONNRESET >> return (-1))
                (\l -> poke pbytes (fromIntegral l) >> return 0)


#elif defined(OSX)
data SendFileCallLog = SendFileCallLog {
      _sf_fd1 :: Fd
    , _sf_fd2 :: Fd
    , _sf_off :: COff
    , _sf_sz  :: COff
    }
  deriving (Eq, Show, Ord)


------------------------------------------------------------------------------
sendFileMockSendFunc :: MVar [IO CInt]          -- ^ sample outputs
                     -> MVar [SendFileCallLog]   -- ^ log of calls
                     -> Fd -> Fd -> COff -> Ptr COff -> IO CInt
sendFileMockSendFunc sampleData callLog !fd1 !fd2 !off !pnbytes = do
    !clen <- peek pnbytes
    modifyMVar_ callLog (return . (++ [SendFileCallLog fd1 fd2 off clen]))
    x <- modifyMVar sampleData $ \xs -> return $!
            if null xs then ([], return Nothing) else (tail xs, fmap Just $! head xs)
    x >>= maybe (c_set_errno eCONNRESET >> return (-1))
                (\l -> do when (l > 0) (poke pnbytes (fromIntegral l))
                          return l)

#endif


------------------------------------------------------------------------------
testSendFile :: Test
testSendFile = testCase "sendfile/sendfile-impl" $ do
    callLog <- newMVar []
    sampleData <- newMVar sampleActions
    nWaits <- newMVar (0 :: Int)
    let bumpWaits = \x -> x `seq` modifyMVar_ nWaits (return . (+1))
    SFI.sendFileImpl (sendFileMockSendFunc sampleData callLog) bumpWaits
                       100 101 0 10
    [c1, c2] <- readMVar callLog
    assertEqual "sendFile1" c1 c2
    assertEqual "sendFile2" 10 (_sf_sz c2)
    readMVar nWaits >>= assertEqual "sendFile3" 1

  where
    sampleActions = [ c_set_errno eAGAIN >> return (-1)
                    , c_set_errno eOK >> return 2
                    ]


------------------------------------------------------------------------------
testSendFileZero :: Test
testSendFileZero = testCase "sendfile/sendfile-zero" $ do
    callLog <- newMVar []
    sampleData <- newMVar sampleActions
    nWaits <- newMVar (0 :: Int)
    let bumpWaits = \x -> x `seq` modifyMVar_ nWaits (return . (+1))
    c <- SFI.sendFileImpl (sendFileMockSendFunc sampleData callLog) bumpWaits
                           100 101 0 0
    readMVar callLog >>= assertEqual "empty call log" []
    readMVar nWaits >>= assertEqual "no waits" 0
    assertEqual "no bytes read" 0 c

  where
    sampleActions = [ c_set_errno eAGAIN >> return (-1)
                    , c_set_errno eOK >> return 2
                    ]


------------------------------------------------------------------------------
testSendFileCrash :: Test
testSendFileCrash = testCase "sendfile/sendFile/crash" $ do
    callLog <- newMVar []
    sampleData <- newMVar sampleActions
    nWaits <- newMVar (0 :: Int)
    let bumpWaits = \x -> x `seq` modifyMVar_ nWaits (return . (+1))
    expectException $
        SFI.sendFileImpl (sendFileMockSendFunc sampleData callLog) bumpWaits
                         100 101 0 10
  where
    sampleActions = [ c_set_errno eCONNRESET >> return (-1) ]
