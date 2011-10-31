{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- | Darwin system-dependent code for 'sendfile'.
module System.SendFile.Darwin (sendFile) where

import Data.Int
import Foreign.C.Error (eAGAIN, eINTR, getErrno, throwErrno)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CInt(CInt))
#else
import Foreign.C.Types (CInt)
#endif
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
#if __GLASGOW_HASKELL__ >= 703
import System.Posix.Types (Fd(Fd), COff(COff))
#else
import System.Posix.Types (Fd, COff)
#endif

sendFile :: IO () -> Fd -> Fd -> Int64 -> Int64 -> IO Int64
sendFile onBlock out_fd in_fd off count
  | count == 0 = return 0
  | otherwise  = alloca $ \pbytes -> do
        poke pbytes $ min maxBytes (fromIntegral count)
        sbytes <- sendfile onBlock out_fd in_fd (fromIntegral off) pbytes
        return $ fromIntegral sbytes

sendfile :: IO () -> Fd -> Fd -> COff -> Ptr COff -> IO COff
sendfile onBlock out_fd in_fd off pbytes = do
    status <- c_sendfile out_fd in_fd off pbytes
    nsent <- peek pbytes
    if status == 0
      then return nsent
      else do errno <- getErrno
              if (errno == eAGAIN) || (errno == eINTR)
                then do
                    if nsent == 0
                      then onBlock >> sendfile onBlock out_fd in_fd off pbytes
                      else return nsent
                else throwErrno "System.SendFile.Darwin"

-- max num of bytes in one send
maxBytes :: COff
maxBytes = maxBound :: COff

-- in Darwin sendfile gives LFS support (no sendfile64 routine)
foreign import ccall unsafe "sys/uio.h sendfile" c_sendfile_darwin
    :: Fd -> Fd -> COff -> Ptr COff -> Ptr () -> CInt -> IO CInt

c_sendfile :: Fd -> Fd -> COff -> Ptr COff -> IO CInt
c_sendfile out_fd in_fd off pbytes =
    c_sendfile_darwin in_fd out_fd off pbytes nullPtr 0
