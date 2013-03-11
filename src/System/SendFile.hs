{-# LANGUAGE CPP #-}

-- | Snap's unified interface to sendfile.
-- Modified from sendfile 0.6.1

module System.SendFile
  ( sendFile
  , sendFileMode
  ) where

#if defined(LINUX)
import           System.SendFile.Linux   (sendFile)
#elif defined(FREEBSD)
import           System.SendFile.FreeBSD (sendFile)
#elif defined(OSX)
import           System.SendFile.Darwin  (sendFile)
#endif

#if defined(LINUX)
sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"
#else if defined(FREEBSD)
sendFileMode :: String
sendFileMode = "FREEBSD_SENDFILE"
#else if defined(OSX)
sendFileMode :: String
sendFileMode = "DARWIN_SENDFILE"
#endif
