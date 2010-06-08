{-# LANGUAGE ForeignFunctionInterface #-}
-- | Darwin system-dependent code for 'sendfile'.
module System.SendFile.Darwin (sendFile) where

import Data.Int
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types (CInt)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import System.Posix.Types (Fd, COff)

sendFile :: Fd -> Fd -> Int64 -> Int64 -> IO Int64
sendFile out_fd in_fd off count
  | count == 0 = return 0
  | otherwise  = alloca $ \pbytes -> do
        poke pbytes $ min maxBytes (fromIntegral count)
        sbytes <- sendfile out_fd in_fd (fromIntegral off) pbytes
        return $ fromIntegral sbytes

sendfile :: Fd -> Fd -> COff -> Ptr COff -> IO COff
sendfile out_fd in_fd off pbytes = do
    status <- c_sendfile out_fd in_fd off pbytes
    nsent <- peek pbytes
    if status == 0
      then return nsent
      else do errno <- getErrno
              if (errno == eAGAIN) || (errno == eINTR)
                then return nsent
                else throwErrno "System.SendFile.Darwin"

-- max num of bytes in one send
maxBytes :: COff
maxBytes = maxBound :: COff

-- in Darwin sendfile gives LFS support (no sendfile64 routine)
foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_darwin
    :: Fd -> Fd -> COff -> Ptr COff -> Ptr () -> CInt -> IO CInt

c_sendfile :: Fd -> Fd -> COff -> Ptr COff -> IO CInt
c_sendfile out_fd in_fd off pbytes = c_sendfile_darwin in_fd out_fd off pbytes nullPtr 0
