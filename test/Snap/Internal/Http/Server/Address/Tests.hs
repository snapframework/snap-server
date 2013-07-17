{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.Address.Tests (tests) where

------------------------------------------------------------------------------
import           Network.Socket
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                        hiding (Test, path)
------------------------------------------------------------------------------
import           Snap.Internal.Http.Server.Address
import           Snap.Test.Common                  (coverShowInstance,
                                                    coverTypeableInstance,
                                                    expectException)


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testGetNameInfoFails
        , testGetAddressBadType
        , testGetAddressIPv6
        , testGetSockAddr
        , testTrivials
        ]


------------------------------------------------------------------------------
testGetNameInfoFails :: Test
testGetNameInfoFails = testCase "address/getNameInfo-fails" $ do
    x <- getHostAddrImpl (\_ _ _ _ -> return (Nothing, Nothing)) undefined
    assertEqual "when getNameInfo fails, getHostAddr should return empty" "" x


------------------------------------------------------------------------------
testGetAddressBadType :: Test
testGetAddressBadType = testCase "address/getAddress-bad-type" $
    expectException $ getAddress $ SockAddrUnix "foo"


------------------------------------------------------------------------------
testGetAddressIPv6 :: Test
testGetAddressIPv6 = testCase "address/getAddress-IPv6" $ do
    let x = SockAddrInet6 10 undefined undefined undefined
    (y, _) <- getAddressImpl (const $ return "") x
    assertEqual "ipv6 port" 10 y


------------------------------------------------------------------------------
testGetSockAddr :: Test
testGetSockAddr = testCase "address/getSockAddr" $ do
    (f1, a1) <- getSockAddr 10 "*"
    assertEqual "" f1 AF_INET
    assertEqual "" a1 $ SockAddrInet 10 iNADDR_ANY

    (f2, a2) <- getSockAddr 10 "::"
    assertEqual "" f2 AF_INET6
    assertEqual "" a2 $ SockAddrInet6 10 0 iN6ADDR_ANY 0

    expectException $ getSockAddrImpl (\_ _ _ -> return []) 10 "foo"


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "address/trivials" $ do
    coverTypeableInstance (undefined :: AddressNotSupportedException)
    coverShowInstance (AddressNotSupportedException "ok")
