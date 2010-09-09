module Main where

import           Control.Concurrent (killThread)
import           Test.Framework (defaultMain, testGroup)



import qualified Data.Concurrent.HashMap.Tests
import qualified Snap.Internal.Http.Parser.Tests
import qualified Snap.Internal.Http.Server.Tests
import qualified Test.Blackbox

main :: IO ()
main = do
    (tid,pt) <- Test.Blackbox.startTestServer
    defaultMain $ tests pt
    killThread tid

  where tests pt =
            [ testGroup "Data.Concurrent.HashMap.Tests"
                        Data.Concurrent.HashMap.Tests.tests
            , testGroup "Snap.Internal.Http.Parser.Tests"
                        Snap.Internal.Http.Parser.Tests.tests
            , testGroup "Snap.Internal.Http.Server.Tests"
                        Snap.Internal.Http.Server.Tests.tests
            , testGroup "Test.Blackbox"
                        $ Test.Blackbox.tests pt
            ]
