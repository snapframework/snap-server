{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Internal.Http.Server.Socket.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Applicative               ((<$>))
import qualified Network.Socket                    as N
------------------------------------------------------------------------------
import           Control.Concurrent                (forkIO, killThread, newEmptyMVar, putMVar, readMVar, takeMVar)
import qualified Control.Exception                 as E
import           Data.IORef                        (newIORef, readIORef, writeIORef)
import           Test.Framework                    (Test)
import           Test.Framework.Providers.HUnit    (testCase)
import           Test.HUnit                        (assertEqual)
------------------------------------------------------------------------------
import qualified Snap.Internal.Http.Server.Socket  as Sock
import           Snap.Test.Common                  (eatException, expectException, withSock)
------------------------------------------------------------------------------
#ifdef HAS_UNIX_SOCKETS
import           System.Directory                  (getTemporaryDirectory)
import           System.FilePath                   ((</>))
import qualified System.Posix                      as Posix
# if !MIN_VERSION_unix(2,6,0)
import           Control.Monad.State               (replicateM)
import           Control.Monad.Trans.State.Strict  as State
import qualified Data.Vector.Unboxed               as V
import           System.Directory                  (createDirectoryIfMissing)
import           System.Random                     (StdGen, newStdGen, randomR)
# endif
#else
import           Snap.Internal.Http.Server.Address (AddressNotSupportedException)
#endif

------------------------------------------------------------------------------
#ifdef HAS_UNIX_SOCKETS
mkdtemp :: String -> IO FilePath
# if MIN_VERSION_unix(2,6,0)
mkdtemp = Posix.mkdtemp

# else

tMPCHARS :: V.Vector Char
tMPCHARS = V.fromList $! ['a'..'z'] ++ ['0'..'9']

mkdtemp template = do
    suffix <- newStdGen >>= return . State.evalState (chooseN 8 tMPCHARS)
    let dir = template ++ suffix
    createDirectoryIfMissing False dir
    return dir
  where
    choose :: V.Vector Char -> State.State StdGen Char
    choose v = do let sz = V.length v
                  idx <- State.state $ randomR (0, sz - 1)
                  return $! (V.!) v idx

    chooseN :: Int -> V.Vector Char -> State.State StdGen String
    chooseN n v = replicateM n $ choose v
#endif
#endif

------------------------------------------------------------------------------
tests :: [Test]
tests = [
          testUnixSocketBind
#if !MIN_VERSION_network(3,0,0)
        , testAcceptFailure
        , testSockClosedOnListenException
#endif
        ]

------------------------------------------------------------------------------
-- TODO: fix these tests which rely on deprecated socket apis
#if !MIN_VERSION_network(3,0,0)
testSockClosedOnListenException :: Test
testSockClosedOnListenException = testCase "socket/closedOnListenException" $ do
    ref <- newIORef Nothing
    expectException $ Sock.bindSocketImpl (sso ref) bs ls "127.0.0.1" 4444
    (Just sock) <- readIORef ref
    let (N.MkSocket _ _ _ _ mvar) = sock
    readMVar mvar >>= assertEqual "socket closed" N.Closed

  where
    sso ref sock _ _ = do
        let (N.MkSocket _ _ _ _ mvar) = sock
        readMVar mvar >>= assertEqual "socket not connected" N.NotConnected
        writeIORef ref (Just sock) >> fail "set socket option"
    bs _ _ = fail "bindsocket"
    ls _ _ = fail "listen"

------------------------------------------------------------------------------
testAcceptFailure :: Test
testAcceptFailure = testCase "socket/acceptAndInitialize" $ do
    sockmvar <- newEmptyMVar
    donemvar <- newEmptyMVar
    E.bracket (Sock.bindSocket "127.0.0.1" $ fromIntegral N.aNY_PORT)
              (N.close)
              (\s -> do
                   p <- fromIntegral <$> N.socketPort s
                   forkIO $ server s sockmvar donemvar
                   E.bracket (forkIO $ client p)
                             (killThread)
                             (\_ -> do
                                csock <- takeMVar sockmvar
                                takeMVar donemvar
                                N.isConnected csock >>=
                                    assertEqual "closed" False
                             )
              )
  where
    server sock sockmvar donemvar = serve `E.finally` putMVar donemvar ()
      where
        serve = eatException $ E.mask $ \restore ->
                Sock.acceptAndInitialize sock restore $ \(csock, _) -> do
                  putMVar sockmvar csock
                  fail "error"

    client port = withSock port (const $ return ())
#endif

testUnixSocketBind :: Test
#ifdef HAS_UNIX_SOCKETS
testUnixSocketBind = testCase "socket/unixSocketBind" $
  withSocketPath $ \path ->  do
#if !MIN_VERSION_network(3,0,0)
    E.bracket (Sock.bindUnixSocket Nothing path) N.close $ \sock -> do
        N.isListening sock >>= assertEqual "listening" True
#endif

    expectException $ E.bracket (Sock.bindUnixSocket Nothing "a/relative/path")
                    N.close doNothing

    expectException $ E.bracket (Sock.bindUnixSocket Nothing "/relative/../path")
                    N.close doNothing

    expectException $ E.bracket (Sock.bindUnixSocket Nothing "/hopefully/not/existing/path")
                    N.close doNothing

#ifdef LINUX
    -- Most (all?) BSD systems ignore access mode on unix sockets.
    -- Should we still check it?

    -- This is pretty much for 100% coverage
    expectException $ E.bracket (Sock.bindUnixSocket Nothing "/")
                    N.close doNothing

    let mode = 0o766
    E.bracket (Sock.bindUnixSocket (Just mode) path) N.close $ \_ -> do
        -- Should check sockFd instead of path?
        sockMode <- fmap Posix.fileMode $ Posix.getFileStatus path
        assertEqual "access mode" (fromIntegral mode) $
            Posix.intersectFileModes Posix.accessModes sockMode
#endif
  where
    doNothing _ = return ()
    withSocketPath act = do
      tmpRoot <- getTemporaryDirectory
      tmpDir <- mkdtemp $ tmpRoot </> "snap-server-test-"
      let path = tmpDir </> "unixSocketBind.sock"
      E.finally (act path) $ do
          eatException $ Posix.removeLink path
          eatException $ Posix.removeDirectory tmpDir

#else
testUnixSocketBind = testCase "socket/unixSocketBind" $ do
    caught <- E.catch (Sock.bindUnixSocket Nothing "/tmp/snap-sock.sock" >> return False)
              $ \(e :: AddressNotSupportedException) -> length (show e) `seq` return True
    assertEqual "not supported" True caught

#endif
