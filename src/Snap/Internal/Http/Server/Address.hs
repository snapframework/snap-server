{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Snap.Internal.Http.Server.Address
  ( getHostAddr
  , getSockAddr
  , getAddress
  ) where

------------------------------------------------------------------------------
import           Network.Socket
import           Data.Maybe
import           Control.Monad
import           Control.Exception
import           Data.Typeable
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Char8 ()
import           Data.ByteString.Internal (c2w, w2c)

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
    return (fromIntegral port, S.pack $ map c2w host)

------------------------------------------------------------------------------
getSockAddr :: Int
            -> ByteString
            -> IO (Family, SockAddr)
getSockAddr p s | s == "*" = ipV4Addr p iNADDR_ANY
getSockAddr p s | s == "::" = ipV6Addr p iN6ADDR_ANY
getSockAddr p s = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST] }
    ai <- getAddrInfo (Just hints) (Just $ map w2c $ S.unpack s) Nothing
    if ai == [] then throwIO $ AddressNotSupportedException $ show s
      else do
        case addrAddress $ head ai of
          SockAddrInet _ h -> ipV4Addr p h
          SockAddrInet6 _ _ h _ -> ipV6Addr p h
          x -> throwIO $ AddressNotSupportedException $ show x

ipV4Addr :: Int -> HostAddress -> IO (Family, SockAddr)
ipV4Addr p h = return (AF_INET, SockAddrInet (fromIntegral p) h)

ipV6Addr :: Int -> HostAddress6 -> IO (Family, SockAddr)
ipV6Addr p h = return (AF_INET6, SockAddrInet6 (fromIntegral p) 0 h 0)
