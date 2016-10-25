{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | FreeBSD system-dependent code for 'sendfile'.
module System.SendFile.FreeBSD
  ( sendFile
  , sendFileImpl
  , sendFileMode
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent    (threadWaitWrite)
import           Data.Int
import           Data.Word
import           Foreign.C.Error       (throwErrnoIfMinus1RetryMayBlock_)
#if __GLASGOW_HASKELL__ >= 703
import           Foreign.C.Types       (CInt (..), CSize (..))
#else
import           Foreign.C.Types       (CInt, CSize)
#endif
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr           (Ptr, nullPtr)
import           Foreign.Storable      (peek)
#if __GLASGOW_HASKELL__ >= 703
import           System.Posix.Types    (COff (..), Fd (..))
#else
import           System.Posix.Types    (COff, Fd)
#endif
------------------------------------------------------------------------------


------------------------------------------------------------------------------
sendFile :: Fd -> Fd -> Word64 -> Word64 -> IO Int64
sendFile = sendFileImpl c_sendfile_freebsd threadWaitWrite
{-# INLINE sendFile #-}


------------------------------------------------------------------------------
sendFileImpl :: (Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt
                    -> IO CInt)
             -> (Fd -> IO ())
             -> Fd -> Fd -> Word64 -> Word64 -> IO Int64
sendFileImpl !rawSendFile !wait out_fd in_fd off count
  | count == 0 = return 0
  | otherwise  = alloca $ \pbytes -> do
        sbytes <- sendfile rawSendFile wait out_fd in_fd
                           (fromIntegral off)
                           (fromIntegral count)
                           pbytes
        return $ fromIntegral sbytes


------------------------------------------------------------------------------
sendfile :: (Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt
                -> IO CInt)
         -> (Fd -> IO ())
         -> Fd -> Fd -> COff -> CSize -> Ptr COff -> IO COff
sendfile rawSendFile wait out_fd in_fd off count pbytes = do
    throwErrnoIfMinus1RetryMayBlock_
        "sendfile"
        (rawSendFile in_fd out_fd off count nullPtr pbytes 0)
        (wait out_fd)
    peek pbytes


------------------------------------------------------------------------------
-- max num of bytes in one send
maxBytes :: CSize
maxBytes = maxBound :: CSize


------------------------------------------------------------------------------
foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_freebsd
    :: Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt -> IO CInt


------------------------------------------------------------------------------
sendFileMode :: String
sendFileMode = "FREEBSD_SENDFILE"
