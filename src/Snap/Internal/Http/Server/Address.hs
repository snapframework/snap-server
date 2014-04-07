{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Snap.Internal.Http.Server.Address
  ( getHostAddr
  , getHostAddrImpl
  , getSockAddr
  , getSockAddrImpl
  , getAddress
  , getAddressImpl
  , AddressNotSupportedException(..)
  ) where

------------------------------------------------------------------------------
import           Control.Exception     (Exception, throwIO)
import           Control.Monad         (liftM)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S (pack, unpack)
import           Data.Maybe            (fromMaybe)
import           Data.Typeable         (Typeable)
import           Network.Socket        (AddrInfo (addrAddress, addrFamily, addrFlags), AddrInfoFlag (AI_NUMERICSERV), Family (AF_INET, AF_INET6), HostName, NameInfoFlag (NI_NUMERICHOST), ServiceName, SockAddr (SockAddrInet, SockAddrInet6), defaultHints, getAddrInfo, getNameInfo, iN6ADDR_ANY, iNADDR_ANY)


------------------------------------------------------------------------------
data AddressNotSupportedException = AddressNotSupportedException String
   deriving (Typeable)

instance Show AddressNotSupportedException where
    show (AddressNotSupportedException x) = "Address not supported: " ++ x

instance Exception AddressNotSupportedException

------------------------------------------------------------------------------
getHostAddr :: SockAddr -> IO String
getHostAddr = getHostAddrImpl getNameInfo


------------------------------------------------------------------------------
getHostAddrImpl :: ([NameInfoFlag]
                    -> Bool
                    -> Bool
                    -> SockAddr
                    -> IO (Maybe HostName, Maybe ServiceName))
                -> SockAddr
                -> IO String
getHostAddrImpl !_getNameInfo addr =
    (fromMaybe "" . fst) `liftM` _getNameInfo [NI_NUMERICHOST] True False addr


------------------------------------------------------------------------------
getAddress :: SockAddr -> IO (Int, ByteString)
getAddress = getAddressImpl getHostAddr


------------------------------------------------------------------------------
getAddressImpl :: (SockAddr -> IO String) -> SockAddr -> IO (Int, ByteString)
getAddressImpl !_getHostAddr addr = do
    port <- case addr of
              SockAddrInet p _ -> return p
              SockAddrInet6 p _ _ _ -> return p
              x -> throwIO $ AddressNotSupportedException $ show x
    host <- _getHostAddr addr
    return (fromIntegral port, S.pack host)

------------------------------------------------------------------------------
getSockAddr :: Int
            -> ByteString
            -> IO (Family, SockAddr)
getSockAddr = getSockAddrImpl getAddrInfo


------------------------------------------------------------------------------
getSockAddrImpl
  :: (Maybe AddrInfo -> Maybe String -> Maybe String -> IO [AddrInfo])
     -> Int -> ByteString -> IO (Family, SockAddr)
getSockAddrImpl !_getAddrInfo p s =
    case () of
      !_ | s == "*" -> return $! ( AF_INET
                                 , SockAddrInet (fromIntegral p) iNADDR_ANY
                                 )
         | s == "::" -> return $! ( AF_INET6
                                  , SockAddrInet6 (fromIntegral p) 0 iN6ADDR_ANY 0
                                  )
         | otherwise -> do ais <- _getAddrInfo (Just hints) (Just $ S.unpack s)
                                               (Just $ show p)
                           if null ais
                             then throwIO $ AddressNotSupportedException $ show s
                             else do
                               let ai = head ais
                               let fm = addrFamily ai
                               let sa = addrAddress ai
                               return (fm, sa)
  where
    hints = defaultHints { addrFlags = [AI_NUMERICSERV] }
