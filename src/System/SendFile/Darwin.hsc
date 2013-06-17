{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Darwin system-dependent code for 'sendfile'.
module System.SendFile.Darwin
  ( sendFile
  , sendFileImpl
  , sendFileMode
  ) where

import           Control.Concurrent (threadWaitWrite)
import           Data.Int
import           Foreign.C.Error    (throwErrnoIfMinus1RetryMayBlock_)
#if __GLASGOW_HASKELL__ >= 703
import           Foreign.C.Types    (CInt (CInt))
#else
import           Foreign.C.Types    (CInt)
#endif
import           Foreign.Marshal    (alloca)
import           Foreign.Ptr        (Ptr, nullPtr)
import           Foreign.Storable   (peek, poke)
#if __GLASGOW_HASKELL__ >= 703
import           System.Posix.Types (COff (COff), Fd (Fd))
#else
import           System.Posix.Types (COff, Fd)
#endif

------------------------------------------------------------------------------
sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO Int64
sendFile = sendFileImpl c_sendfile threadWaitWrite
{-# INLINE sendFile #-}


------------------------------------------------------------------------------
sendFileImpl :: (Fd -> Fd -> COff -> Ptr COff -> IO CInt)
             -> (Fd -> IO ())
             -> Fd -> Fd -> Int64 -> Int64 -> IO Int64
sendFileImpl rawSendFile wait out_fd in_fd off count
  | count == 0 = return 0
  | otherwise  = alloca $ \pbytes -> do
        poke pbytes $ fromIntegral count
        sbytes <- sendfile rawSendFile wait out_fd in_fd (fromIntegral off)
                           pbytes
        return $ fromIntegral sbytes
{-# INLINE sendFileImpl #-}


------------------------------------------------------------------------------
sendfile :: (Fd -> Fd -> COff -> Ptr COff -> IO CInt)
         -> (Fd -> IO ())
         -> Fd -> Fd -> COff -> Ptr COff -> IO COff
sendfile rawSendFile wait out_fd in_fd off pbytes = do
    throwErrnoIfMinus1RetryMayBlock_ "sendfile"
                                     (rawSendFile out_fd in_fd off pbytes)
                                     (wait out_fd)
    peek pbytes
{-# INLINE sendfile #-}

------------------------------------------------------------------------------
-- in Darwin sendfile gives LFS support (no sendfile64 routine)
foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_darwin
    :: Fd -> Fd -> COff -> Ptr COff -> Ptr () -> CInt -> IO CInt


------------------------------------------------------------------------------
c_sendfile :: Fd -> Fd -> COff -> Ptr COff -> IO CInt
c_sendfile out_fd in_fd off pbytes =
    c_sendfile_darwin in_fd out_fd off pbytes nullPtr 0
{-# INLINE c_sendfile #-}


------------------------------------------------------------------------------
sendFileMode :: String
sendFileMode = "DARWIN_SENDFILE"
