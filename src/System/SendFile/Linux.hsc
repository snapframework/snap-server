{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Linux system-dependent code for 'sendfile'.
module System.SendFile.Linux (sendFile) where

import Data.Int
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CSize(..), CInt(..))
#else
import Foreign.C.Types (CSize)
#endif
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (poke)
#if __GLASGOW_HASKELL__ >= 703
import System.Posix.Types (Fd(..), COff(..), CSsize(..))
#else
import System.Posix.Types (Fd, COff, CSsize)
#endif

sendFile :: IO () -> Fd -> Fd -> Int64 -> Int64 -> IO Int64
sendFile onBlock out_fd in_fd off count
  | count == 0 = return 0
  | off == 0   = do
        sbytes <- sendfile onBlock out_fd in_fd nullPtr bytes
        return $ fromIntegral sbytes
  | otherwise  = alloca $ \poff -> do
        poke poff (fromIntegral off)
        sbytes <- sendfile onBlock out_fd in_fd poff bytes
        return $ fromIntegral sbytes
    where
      bytes = min (fromIntegral count) maxBytes

sendfile :: IO () -> Fd -> Fd -> Ptr COff -> CSize -> IO CSsize
sendfile onBlock out_fd in_fd poff bytes = do
    nsent <- c_sendfile out_fd in_fd poff bytes
    if nsent <= -1
      then do errno <- getErrno
              if errno == eAGAIN
                then onBlock >> sendfile onBlock out_fd in_fd poff bytes
                else throwErrno "System.SendFile.Linux"
      else return nsent

-- max num of bytes in one send
maxBytes :: CSize
maxBytes = maxBound :: CSize

-- sendfile64 gives LFS support
foreign import ccall unsafe "sys/sendfile.h sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize
