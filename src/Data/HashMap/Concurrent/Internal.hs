{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HashMap.Concurrent.Internal where

import           Data.Bits
import           Data.Word
import           Language.Haskell.TH


whichHash :: ExpQ -> ExpQ -> Q Exp
whichHash as32 as64 = if bitSize (undefined :: Word) == 32
                         then [| \x -> fromIntegral $ $as32 x |]
                         else [| \x -> fromIntegral $ $as64 x |]
