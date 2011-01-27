{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Snap.Internal.Http.Server.TimeoutTable
  ( TimeoutTable
  , new
  , null
  , insert
  , delete
  , killAll
  , killOlderThan
  , waitForActivity
  )
where


------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Data.Bits
import qualified Data.PSQueue as PSQ
import           Data.PSQueue (PSQ)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Word
import           Foreign.C.Types (CTime)
import           GHC.Conc (numCapabilities)
import           Prelude hiding (null)
------------------------------------------------------------------------------
import           Data.Concurrent.HashMap (nextHighestPowerOf2)


------------------------------------------------------------------------------
type TT = PSQ ThreadId CTime


------------------------------------------------------------------------------
data TimeoutTable = TimeoutTable {
      _maps     :: !(Vector (MVar TT))
    , _activity :: !(MVar ())
}


------------------------------------------------------------------------------
defaultNumberOfLocks :: Word
defaultNumberOfLocks = nextHighestPowerOf2 $ toEnum $ 8 * numCapabilities


------------------------------------------------------------------------------
hashToBucket :: Word -> Word
hashToBucket x = x .&. (defaultNumberOfLocks-1)


------------------------------------------------------------------------------
new :: IO TimeoutTable
new = do
    vector <- V.replicateM (fromEnum defaultNumberOfLocks) (newMVar PSQ.empty)
    act    <- newEmptyMVar
    return $ TimeoutTable vector act


------------------------------------------------------------------------------
null :: TimeoutTable -> IO Bool
null (TimeoutTable maps _) = do
    nulls <- V.mapM (\mv -> withMVar mv $ return . PSQ.null) maps
    return $ V.and nulls


------------------------------------------------------------------------------
insert :: Word -> ThreadId -> CTime -> TimeoutTable -> IO ()
insert thash tid time (TimeoutTable maps act) = do
    modifyMVar_ psqMv $ \psq -> do
        let !psq' = PSQ.insert tid time psq
        return $! psq'

    _ <- tryPutMVar act ()
    return ()

  where
    bucket = hashToBucket thash
    psqMv  = V.unsafeIndex maps $ fromEnum bucket


------------------------------------------------------------------------------
delete :: Word -> ThreadId -> TimeoutTable -> IO ()
delete thash tid (TimeoutTable maps act) = do
    modifyMVar_ psqMv $ \psq -> do
        let !psq' = PSQ.delete tid psq
        return $! psq'

    _ <- tryPutMVar act ()
    return ()

  where
    bucket = hashToBucket thash
    psqMv  = V.unsafeIndex maps $ fromEnum bucket


------------------------------------------------------------------------------
killAll :: TimeoutTable -> IO ()
killAll (TimeoutTable maps _) = do
    V.mapM_ k maps

  where
    k psqMV = modifyMVar_ psqMV $ \psq -> do
        mapM_ killThread $ PSQ.keys psq
        return PSQ.empty


------------------------------------------------------------------------------
killOlderThan :: CTime -> TimeoutTable -> IO ()
killOlderThan time (TimeoutTable maps _) = do
    V.mapM_ processPSQ maps

  where
    processPSQ psqMV = modifyMVar_ psqMV $ \psq -> do
        let (psq', threads) = findOlder psq []
        mapM_ killThread threads
        return psq'

    findOlder psq l =
        let mmin = PSQ.findMin psq
        in maybe (psq,l)
                 (\m -> if PSQ.prio m <= time
                          then findOlder (PSQ.deleteMin psq) ((PSQ.key m):l)
                          else (psq,l))
                 mmin


------------------------------------------------------------------------------
waitForActivity :: TimeoutTable -> IO ()
waitForActivity t@(TimeoutTable _ act) = do
    takeMVar act
    b <- null t

    -- if the table is not empty, put the activity mvar back
    unless b $ (tryPutMVar act () >> return ())

    threadDelay 2500000
