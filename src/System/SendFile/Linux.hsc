{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

------------------------------------------------------------------------------
-- | Linux system-dependent code for 'sendfile'.
module System.SendFile.Linux
  ( sendFile
  , sendFileImpl
  , sendFileMode
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent (threadWaitWrite)
import           Data.Int           (Int64)
import           Data.Word          (Word64)
import           Foreign.C.Error    (throwErrnoIfMinus1RetryMayBlock)
#if __GLASGOW_HASKELL__ >= 703
import           Foreign.C.Types    (CInt (..), CSize (..))
#else
import           Foreign.C.Types    (CSize)
#endif
import           Foreign.Marshal    (alloca)
import           Foreign.Ptr        (Ptr, nullPtr)
import           Foreign.Storable   (poke)
#if __GLASGOW_HASKELL__ >= 703
import           System.Posix.Types (COff (..), CSsize (..), Fd (..))
#else
import           System.Posix.Types (COff, CSsize, Fd)
#endif


------------------------------------------------------------------------------
sendFile :: Fd -> Fd -> Word64 -> Word64 -> IO Int64
sendFile = sendFileImpl c_sendfile threadWaitWrite
{-# INLINE sendFile #-}


------------------------------------------------------------------------------
sendFileImpl :: (Fd -> Fd -> Ptr COff -> CSize -> IO CSsize)
             -> (Fd -> IO ())
             -> Fd -> Fd -> Word64 -> Word64 -> IO Int64
sendFileImpl !raw_sendfile !wait out_fd in_fd off count
  | count <= 0 = return 0
  | off   == 0 = do
        nsent <- sendfile raw_sendfile wait out_fd in_fd nullPtr bytes
        return $! fromIntegral nsent
  | otherwise  = alloca $ \poff -> do
        poke poff (fromIntegral off)
        nsent <- sendfile raw_sendfile wait out_fd in_fd poff bytes
        return $! fromIntegral nsent
    where
      bytes = fromIntegral count
{-# INLINE sendFileImpl #-}


------------------------------------------------------------------------------
sendfile :: (Fd -> Fd -> Ptr COff -> CSize -> IO CSsize)
         -> (Fd -> IO ())
         -> Fd -> Fd -> Ptr COff -> CSize -> IO CSsize
sendfile raw_sendfile wait out_fd in_fd poff bytes =
    throwErrnoIfMinus1RetryMayBlock
            "sendfile"
            (raw_sendfile out_fd in_fd poff bytes)
            (wait out_fd)
{-# INLINE sendfile #-}


------------------------------------------------------------------------------
-- sendfile64 gives LFS support
foreign import ccall unsafe "sys/sendfile.h sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize


------------------------------------------------------------------------------
sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"
