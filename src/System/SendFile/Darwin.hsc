{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Darwin system-dependent code for 'sendfile'.
module System.SendFile.Darwin (sendFile, sendFileMode) where

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
sendFile out_fd in_fd off count
  | count == 0 = return 0
  | otherwise  = alloca $ \pbytes -> do
        poke pbytes $ fromIntegral count
        sbytes <- sendfile out_fd in_fd (fromIntegral off) pbytes
        return $ fromIntegral sbytes


------------------------------------------------------------------------------
sendfile :: Fd -> Fd -> COff -> Ptr COff -> IO COff
sendfile out_fd in_fd off pbytes = do
    throwErrnoIfMinus1RetryMayBlock_ "sendfile"
                                     (c_sendfile out_fd in_fd off pbytes)
                                     onBlock
    peek pbytes

  where
    onBlock = threadWaitWrite out_fd


------------------------------------------------------------------------------
-- in Darwin sendfile gives LFS support (no sendfile64 routine)
foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_darwin
    :: Fd -> Fd -> COff -> Ptr COff -> Ptr () -> CInt -> IO CInt


------------------------------------------------------------------------------
c_sendfile :: Fd -> Fd -> COff -> Ptr COff -> IO CInt
c_sendfile out_fd in_fd off pbytes =
    c_sendfile_darwin in_fd out_fd off pbytes nullPtr 0


------------------------------------------------------------------------------
sendFileMode :: String
sendFileMode = "DARWIN_SENDFILE"
