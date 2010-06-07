{-# LANGUAGE CPP #-}

-- | Snap's unified interface to sendfile.
-- Modified from sendfile by Matthew Elder

module System.SendFile
  ( sendFile
  , sendFileMode
  ) where

#if defined(LINUX)
import System.SendFile.Linux (sendFile)

sendFileMode :: String
sendFileMode = "LINUX_SENDFILE"
#endif

#if defined(FREEBSD)
import System.SendFile.FreeBSD (sendFile)

sendFileMode :: String
sendFileMode = "FREEBSD_SENDFILE"
#endif

#if defined(OSX)
import System.SendFile.Darwin (sendFile)

sendFileMode :: String
sendFileMode = "DARWIN_SENDFILE"
#endif
