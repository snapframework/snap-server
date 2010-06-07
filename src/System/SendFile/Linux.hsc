{-# LANGUAGE ForeignFunctionInterface #-}
-- | Linux system-dependent code for 'sendfile'.
module System.SendFile.Linux (sendFile) where

import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types (CSize)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (poke)
import System.Posix.Types (Fd, COff, CSsize)

sendFile :: Fd -> Fd -> Int -> Int -> IO Int
sendFile out_fd in_fd off count
  | count == 0 = return 0
  | off == 0   = do
        sbytes <- sendfile out_fd in_fd nullPtr bytes
        return $ fromEnum sbytes
  | otherwise  = alloca $ \poff -> do
        poke poff (fromIntegral off)
        sbytes <- sendfile out_fd in_fd poff bytes
        return $ fromEnum sbytes
    where
      bytes = min (fromIntegral count) maxBytes

sendfile :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize
sendfile out_fd in_fd poff bytes = do
    nsent <- c_sendfile out_fd in_fd poff bytes
    if nsent <= -1
      then do errno <- getErrno
              if errno == eAGAIN
                then sendfile out_fd in_fd poff bytes
                else throwErrno "System.SendFile.Linux"
      else return nsent

-- max num of bytes in one send
maxBytes :: CSize
maxBytes = maxBound :: CSize

-- sendfile64 gives LFS support
foreign import ccall unsafe "sys/sendfile.h sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize
