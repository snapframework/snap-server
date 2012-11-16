{-# LANGUAGE CPP #-}

-- | Snap's unified interface to sendfile.
-- Modified from sendfile 0.6.1

module System.SendFile
  ( sendFile
  , sendFileMode
  ) where

#if defined(LINUX)
import System.SendFile.Linux (sendFile)

sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"
#elif defined(FREEBSD)
import System.SendFile.FreeBSD (sendFile)

sendFileMode :: String
sendFileMode = "FREEBSD_SENDFILE"
#elif defined(OSX)
import System.SendFile.Darwin (sendFile)

sendFileMode :: String
sendFileMode = "DARWIN_SENDFILE"
#endif
