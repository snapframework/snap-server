{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Snap.Internal.Http.Server.Cleanup
  ( Cleanup
  , io
  , cleanup
  , runCleanup
  ) where

------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative              (Applicative, (<$>))
#endif
import qualified Control.Exception                as E
import           Control.Monad                    (join)
import qualified Control.Monad.Reader             as R
import           Control.Monad.Trans              (lift)
import           Data.IORef                       (IORef, modifyIORef, newIORef, readIORef)
import           Snap.Internal.Http.Server.Common (eatException)

------------------------------------------------------------------------------
type WithCleanup a = (IO a, a -> IO ())
data CleanupData = CleanupData {
    _register :: forall r . WithCleanup r -> IO r,
    _restore  :: forall a . IO a -> IO a
    }

------------------------------------------------------------------------------
-- | A simple resource-handling monad for use during server startup.
newtype Cleanup a = Cleanup (R.ReaderT CleanupData IO a)
  deriving (Functor, Applicative, Monad)


------------------------------------------------------------------------------
-- | Allocates some resource and schedules its cleanup action to be run at the end
-- of the whole computation. (Like bracket)
cleanup :: IO a -> (a -> IO ()) -> Cleanup a
cleanup create destroy = Cleanup $ do
    reg <- R.asks _register
    lift $ reg (create, destroy)


------------------------------------------------------------------------------
-- | Run an IO action within the Cleanup monad.
io :: IO a -> Cleanup a
io m = Cleanup $ do
    res <- R.asks _restore
    lift $ res m


------------------------------------------------------------------------------
runCleanup :: Cleanup a -> IO a
runCleanup (Cleanup m) = E.mask $ \restore -> do
    ref    <- newIORef (return $! ())

    let cd = CleanupData (reg restore ref) restore
    x <- R.runReaderT m cd `E.onException` runRef ref
    runRef ref
    E.evaluate x
  where
    runRef ref = join (readIORef ref)

    reg :: forall r .
           (forall a . IO a -> IO a)
        -> IORef (IO ())
        -> WithCleanup r
        -> IO r
    reg restore ref (create, destroy) =
        E.bracketOnError (restore create) destroy $ \v -> do
            modifyIORef ref (`E.finally` eatException (destroy v))
            return $! v
