{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Snap.Internal.Http.Server.Address
  ( getHostAddr
  , getSockAddr
  , getAddress
  , AddressNotSupportedException
  ) where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.Maybe
import           Data.Typeable
import           Network.Socket

------------------------------------------------------------------------------
data AddressNotSupportedException = AddressNotSupportedException String
   deriving (Typeable)

instance Show AddressNotSupportedException where
    show (AddressNotSupportedException x) = "Address not supported: " ++ x

instance Exception AddressNotSupportedException

------------------------------------------------------------------------------
getHostAddr :: SockAddr -> IO String
getHostAddr addr =
    (fromMaybe "" . fst) `liftM` getNameInfo [NI_NUMERICHOST] True False addr

------------------------------------------------------------------------------
getAddress :: SockAddr -> IO (Int, ByteString)
getAddress addr = do
    port <- case addr of
              SockAddrInet p _ -> return p
              SockAddrInet6 p _ _ _ -> return p
              x -> throwIO $ AddressNotSupportedException $ show x
    host <- getHostAddr addr
    return (fromIntegral port, S.pack host)

------------------------------------------------------------------------------
getSockAddr :: Int
            -> ByteString
            -> IO (Family, SockAddr)
getSockAddr p s | s == "*"  =
                    return $! ( AF_INET
                              , SockAddrInet (fromIntegral p) iNADDR_ANY
                              )
getSockAddr p s | s == "::" =
                    return $! ( AF_INET6
                              , SockAddrInet6 (fromIntegral p) 0 iN6ADDR_ANY 0
                              )
getSockAddr p s = do
    let hints = defaultHints { addrFlags = [AI_NUMERICSERV] }
    ais <- getAddrInfo (Just hints) (Just $ S.unpack s)
                       (Just $ show p)
    if null ais
      then throwIO $ AddressNotSupportedException $ show s
      else do
        let ai = head ais
        let fm = addrFamily ai
        let sa = addrAddress ai
        return (fm, sa)
