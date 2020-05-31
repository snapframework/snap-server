{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
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
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative   ((<$>))
#endif
import           Control.Exception     (Exception, throwIO)
import           Control.Monad         (liftM)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.Maybe            (fromMaybe)
import           Data.Typeable         (Typeable)
import           Network.Socket        (AddrInfo (addrAddress, addrFamily, addrFlags, addrSocketType), AddrInfoFlag (AI_NUMERICSERV, AI_PASSIVE), Family (AF_INET, AF_INET6), HostName, NameInfoFlag (NI_NUMERICHOST), ServiceName, SockAddr (SockAddrInet, SockAddrInet6), SocketType (Stream), defaultHints, getAddrInfo, getNameInfo)


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
getAddressImpl !_getHostAddr addr =
  case addr of
    SockAddrInet p _      -> host (fromIntegral p)
    SockAddrInet6 p _ _ _ -> host (fromIntegral p)
#if MIN_VERSION_network(2,6,0)
    _                     -> fail "Unsupported address type"
    where
      host port   = (,) port . S.pack <$> _getHostAddr addr
#else
    SockAddrUnix path     -> return (-1, prefix path)
    where
      prefix path = T.encodeUtf8 $! T.pack $ "unix:" ++ path
#endif


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
      !_ | s == "*" -> getAddrs isIPv4 (Just wildhints) Nothing (Just $ show p)
         | s == "::" -> getAddrs isIPv6 (Just wildhints) Nothing (Just $ show p)
         | otherwise -> getAddrs (const True) (Just hints) (Just $ S.unpack s) (Just $ show p)

  where
    isIPv4 ai = addrFamily ai == AF_INET
    isIPv6 ai = addrFamily ai == AF_INET6

    getAddrs flt a b c = do
        ais <- filter flt <$> _getAddrInfo a b c
        if null ais
          then throwIO $ AddressNotSupportedException $ show s
          else do
            let ai = head ais
            let fm = addrFamily ai
            let sa = addrAddress ai
            return (fm, sa)

    wildhints = hints { addrFlags = [AI_NUMERICSERV, AI_PASSIVE] }
    hints = defaultHints { addrFlags = [AI_NUMERICSERV]
                         , addrSocketType = Stream
                         }
