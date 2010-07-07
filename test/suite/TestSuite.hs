module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Data.HashMap.Concurrent.Tests
import qualified Snap.Internal.Http.Parser.Tests
import qualified Snap.Internal.Http.Server.Tests

main :: IO ()
main = defaultMain tests
  where tests = [ testGroup "Data.HashMap.Concurrent.Tests"
                            Data.HashMap.Concurrent.Tests.tests
                , testGroup "Snap.Internal.Http.Parser.Tests"
                            Snap.Internal.Http.Parser.Tests.tests
                , testGroup "Snap.Internal.Http.Server.Tests"
                            Snap.Internal.Http.Server.Tests.tests
                ]
