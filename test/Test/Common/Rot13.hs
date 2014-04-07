module Test.Common.Rot13 (rot13) where

----------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.Char( ord, isAsciiUpper, isAsciiLower, isAlpha, chr )


------------------------------------------------------------------------------
rotone :: Char -> Char
rotone x | acc x = f
         | otherwise = x
  where
    aA    = ord 'A'
    aa    = ord 'a'
    xx    = ord x
    f     = g $ if isAsciiUpper x then aA else aa
    g st  = chr $ st + (xx - st + 13) `mod` 26
    acc c = isAlpha c && (isAsciiUpper c || isAsciiLower c)


----------------------------------------------------------------------------
rot13 :: ByteString -> ByteString
rot13 = S.map rotone
