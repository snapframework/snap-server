{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Linux system-dependent code for 'sendfile'.
module System.SendFile.Linux
  ( sendFile
  , sendFileMode
  ) where

import           Control.Concurrent (threadWaitWrite)
import           Data.Int
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
sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO Int64
sendFile out_fd in_fd off count
  | count <= 0 = return 0
  | off   == 0 = do
        nsent <- sendfile out_fd in_fd nullPtr bytes
        return $! fromIntegral nsent
  | otherwise  = alloca $ \poff -> do
        poke poff (fromIntegral off)
        nsent <- sendfile out_fd in_fd poff bytes
        return $! fromIntegral nsent
    where
      bytes = fromIntegral count


------------------------------------------------------------------------------
sendfile :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize
sendfile out_fd in_fd poff bytes =
    throwErrnoIfMinus1RetryMayBlock
            "sendfile"
            (c_sendfile out_fd in_fd poff bytes)
            (threadWaitWrite out_fd)


------------------------------------------------------------------------------
-- sendfile64 gives LFS support
foreign import ccall unsafe "sys/sendfile.h sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize


------------------------------------------------------------------------------
sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"
