{-# LANGUAGE ForeignFunctionInterface #-}
-- | FreeBSD system-dependent code for 'sendfile'.
module System.SendFile.FreeBSD (sendFile) where

import Control.Concurrent (threadWaitWrite)
import Data.Int
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.Posix.Types (COff, Fd)

sendFile :: IO () -> Fd -> Fd -> Int64 -> Int64 -> IO Int64
sendFile onBlock out_fd in_fd off count
  | count == 0 = return 0
  | otherwise  = alloca $ \pbytes -> do
        sbytes <- sendfile onBlock out_fd in_fd
                           (fromIntegral off)
                           (fromIntegral count)
                           pbytes
        return $ fromIntegral sbytes

sendfile :: IO () -> Fd -> Fd -> COff -> CSize -> Ptr COff -> IO COff
sendfile onBlock out_fd in_fd off count pbytes = do
    res <- c_sendfile_freebsd in_fd out_fd off count nullPtr pbytes 0
    nsent <- peek pbytes
    if (res == 0)
       then return nsent
       else do
           errno <- getErrno
           if (errno == eAGAIN) || (errno == eINTR)
             then if nsent == 0
                    then do
                        onBlock
                        sendfile onBlock out_fd in_fd off count pbytes
                    else return nsent
             else throwErrno "System.SendFile.FreeBSD.sendfile"

-- max num of bytes in one send
maxBytes :: CSize
maxBytes = maxBound :: CSize

foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_freebsd
    :: Fd -> Fd -> COff -> CSize -> Ptr () -> Ptr COff -> CInt -> IO CInt
