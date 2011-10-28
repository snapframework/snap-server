{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}

module Data.Concurrent.HashMap
  ( HashMap
  , new
  , new'
  , null
  , insert
  , delete
  , lookup
  , update
  , fromList
  , toList
  , hashString
  , hashBS
  , hashInt
  , nextHighestPowerOf2 ) where

------------------------------------------------------------------------------

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.Digest.Murmur32 as Murmur
import qualified Data.Digest.Murmur64 as Murmur
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Maybe
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           GHC.Conc (numCapabilities)
import           Prelude hiding (lookup, null)
import qualified Prelude

#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts ( Word(..), Int(..), shiftRL# )
#else
import Data.Word
#endif

hashString :: String -> Word
hashString = if bitSize (undefined :: Word) == 32
               then fromIntegral . Murmur.asWord32 . Murmur.hash32
               else fromIntegral . Murmur.asWord64 . Murmur.hash64
{-# INLINE hashString #-}


hashInt :: Int -> Word
hashInt = if bitSize (undefined :: Word) == 32
            then fromIntegral . Murmur.asWord32 . Murmur.hash32
            else fromIntegral . Murmur.asWord64 . Murmur.hash64
{-# INLINE hashInt #-}


hashBS :: B.ByteString -> Word
hashBS = if bitSize (undefined :: Word) == 32 then h32 else h64
  where
    h32 s = fromIntegral $ Murmur.asWord32 $
            B.foldl' (\h c -> h `seq` c `seq`
                              Murmur.hash32AddInt (fromEnum c) h)
                     (Murmur.hash32 ([] :: [Int]))
                     s
    h64 s = fromIntegral $ Murmur.asWord64 $
            B.foldl' (\h c -> h `seq` c `seq`
                              Murmur.hash64AddInt (fromEnum c) h)
                     (Murmur.hash64 ([] :: [Int]))
                     s
{-# INLINE hashBS #-}


data HashMap k v = HM {
      _hash         :: !(k -> Word)
    , _hashToBucket :: !(Word -> Word)
    , _maps         :: !(Vector (MVar (Submap k v)))
}



null :: HashMap k v -> IO Bool
null ht = liftM V.and $ V.mapM f $ _maps ht

  where
    f mv = withMVar mv (return . IM.null)


new' :: Eq k =>
        Int            -- ^ number of locks to use
     -> (k -> Word)    -- ^ hash function
     -> IO (HashMap k v)
new' numLocks hashFunc = do
    vector <- V.replicateM (fromEnum n) (newMVar IM.empty)
    return $! HM hf bh vector

  where
    hf !x = hashFunc x
    bh !x = x .&. (n-1)
    !n    = nextHighestPowerOf2 $ toEnum numLocks


new :: Eq k =>
       (k -> Word)      -- ^ hash function
    -> IO (HashMap k v)
new = new' defaultNumberOfLocks


insert :: k -> v -> HashMap k v -> IO ()
insert key value ht =
    modifyMVar_ submap $ \m ->
        return $! insSubmap hashcode key value m

  where
    hashcode = _hash ht key
    bucket   = _hashToBucket ht hashcode
    submap   = V.unsafeIndex (_maps ht) (fromEnum bucket)


delete :: (Eq k) => k -> HashMap k v -> IO ()
delete key ht =
    modifyMVar_ submap $ \m ->
        return $! delSubmap hashcode key m
  where
    hashcode = _hash ht key
    bucket   = _hashToBucket ht hashcode
    submap   = V.unsafeIndex (_maps ht) (fromEnum bucket)


lookup :: (Eq k) => k -> HashMap k v -> IO (Maybe v)
lookup key ht =
    withMVar submap $ \m ->
        return $! lookupSubmap hashcode key m
  where
    hashcode = _hash ht key
    bucket   = _hashToBucket ht hashcode
    submap   = V.unsafeIndex (_maps ht) (fromEnum bucket)


update :: (Eq k) => k -> v -> HashMap k v -> IO Bool
update key value ht =
    modifyMVar submap $ \m ->
        return $! updateSubmap hashcode key value m
  where
    hashcode = _hash ht key
    bucket   = _hashToBucket ht hashcode
    submap   = V.unsafeIndex (_maps ht) (fromEnum bucket)


toList :: HashMap k v -> IO [(k,v)]
toList ht = liftM (concat . V.toList) $ V.mapM f $ _maps ht
  where
    f m = withMVar m $ \sm -> return $ concat $ IM.elems sm


fromList :: (Eq k) => (k -> Word) -> [(k,v)] -> IO (HashMap k v)
fromList hf xs = do
    ht <- new hf
    mapM_ (\(k,v) -> insert k v ht) xs
    return $! ht


------------------------------------------------------------------------------
-- helper functions
------------------------------------------------------------------------------

-- nicked this technique from Data.IntMap

shiftRL :: Word -> Int -> Word
#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
shiftRL (W# x) (I# i)
  = W# (shiftRL# x i)
#else
shiftRL x i   = shiftR x i
#endif


type Submap k v = IntMap [(k,v)]


nextHighestPowerOf2 :: Word -> Word
nextHighestPowerOf2 w = highestBitMask (w-1) + 1


highestBitMask :: Word -> Word
highestBitMask !x0 = case (x0 .|. shiftRL x0 1) of
                      x1 -> case (x1 .|. shiftRL x1 2) of
                       x2 -> case (x2 .|. shiftRL x2 4) of
                        x3 -> case (x3 .|. shiftRL x3 8) of
                         x4 -> case (x4 .|. shiftRL x4 16) of
                          x5 -> x5 .|. shiftRL x5 32



insSubmap :: Word -> k -> v -> Submap k v -> Submap k v
insSubmap hashcode key value m = let !x = f m in x
  where
    f = IM.insertWith (++) (fromIntegral hashcode) [(key,value)]


delSubmap :: (Eq k) => Word -> k -> Submap k v -> Submap k v
delSubmap hashcode key m =
    let !z = IM.update f (fromIntegral hashcode) m in z

  where
    f l = let l' = del l in if Prelude.null l' then Nothing else Just l'

    del = filter ((/= key) . fst)


lookupSubmap :: (Eq k) => Word -> k -> Submap k v -> Maybe v
lookupSubmap hashcode key m = maybe Nothing (Prelude.lookup key) mbBucket
  where
    mbBucket = IM.lookup (fromIntegral hashcode) m


updateSubmap :: (Eq k) => Word -> k -> v -> Submap k v -> (Submap k v, Bool)
updateSubmap hashcode key value m = (m'', b)
  where
    oldV = lookupSubmap hashcode key m
    m'   = maybe m (const $ delSubmap hashcode key m) oldV
    m''  = insSubmap hashcode key value m'
    b    = isJust oldV


defaultNumberOfLocks :: Int
defaultNumberOfLocks = 8 * numCapabilities
