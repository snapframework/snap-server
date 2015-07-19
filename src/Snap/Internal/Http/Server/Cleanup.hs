{-# LANGUAGE RankNTypes #-}
module Snap.Internal.Http.Server.Cleanup
  ( Cleanup
  , WithCleanup
  , io
  , register
  , runCleanup
  ) where

------------------------------------------------------------------------------
import           Control.Applicative              ((<$>))
import qualified Control.Exception                as E
import           Control.Monad.Reader             (ReaderT, ask)
import           Control.Monad.Trans              (lift)
import           Data.IORef                       (IORef, modifyIORef, newIORef, readIORef)
import           Snap.Internal.Http.Server.Common (eatException)

------------------------------------------------------------------------------
type WithCleanup a = (IO a, a -> IO ())
data CleanupData = CleanupData {
    _register :: forall r . WithCleanup r -> IO r,
    _restore  :: forall a . IO a -> IO a
    }
newtype Cleanup a = Cleanup (R.ReaderT CleanupData IO a)
  deriving (Functor, Applicative, Monad)

register :: WithCleanup a -> Cleanup a
register w = Cleanup $ do
    reg <- _register <$> ask
    lift $ reg w

io :: IO a -> Cleanup a
io m = Cleanup $ do
    res <- _restore <$> ask
    lift $ res m

runCleanup :: Cleanup a -> IO (a, IO ())
runCleanup (Cleanup m) = E.mask $ \restore -> do
    ref <- newIORef (return $! ())
    let cd = CleanupData (reg restore ref) restore
    x <- runReaderT m cd `onException` join (readIORef ref)
    act <- readIORef ref
    return $! (x, act)
  where
    reg :: forall r .
           (forall a . IO a -> IO a)
        -> WithCleanup r
        -> IO r
    reg restore ref (create, destroy) =
        E.bracketOnError (restore create) destroy $ \v ->
            modifyIORef ref (>> eatException (destroy v))
