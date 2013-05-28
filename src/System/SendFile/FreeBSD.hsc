{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | FreeBSD system-dependent code for 'sendfile'.
module System.SendFile.FreeBSD (sendFile, sendFileMode) where

------------------------------------------------------------------------------
import           Control.Concurrent    (threadWaitWrite)
import           Data.Int
import           Foreign.C.Error       (throwErrnoIfMinus1RetryMayBlock)
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
sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO Int64
sendFile out_fd in_fd off count
  | count == 0 = return 0
  | otherwise  = alloca $ \pbytes -> do
        sbytes <- sendfile out_fd in_fd
                           (fromIntegral off)
                           (fromIntegral count)
                           pbytes
        return $ fromIntegral sbytes


------------------------------------------------------------------------------
sendfile :: Fd -> Fd -> COff -> CSize -> Ptr COff -> IO COff
sendfile out_fd in_fd off count pbytes = do
    throwErrnoIfMinus1RetryMayBlock_
        "sendfile"
        (c_sendfile_freebsd in_fd out_fd off count nullPtr pbytes 0)
        onBlock
    peek pbytes

  where
    onBlock = threadWaitWrite out_fd

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
