{-# LANGUAGE FlexibleContexts #-}
module Data.WordUtils
    ( module Data.Word
    , wordToFloat, floatToWord, wordToDouble, doubleToWord
    , wordToInt, intToWord
    , combineWord32s, splitWord64
    ) where

import Data.Bits
import Data.Word
import Data.Int
import Data.Array.ST
import Data.Array.Unsafe
import GHC.ST

castST :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
castST x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

wordToFloat :: Word32 -> Float
wordToFloat x = runST $ castST x

floatToWord :: Float -> Word32
floatToWord x = runST $ castST x

wordToDouble :: Word64 -> Double
wordToDouble x = runST $ castST x

doubleToWord :: Double -> Word64
doubleToWord x = runST $ castST x

wordToInt :: Word32 -> Int32
wordToInt = fromIntegral

intToWord :: Int32 -> Word32
intToWord = fromIntegral

-- | Combine two Word32s into one Word64.
-- Big Endian - the first word is the upper 32 bits.
combineWord32s :: (Word32, Word32) -> Word64
combineWord32s (w1, w2) =
    let pad :: Word32 -> Word64
        pad = fromIntegral
     in (pad w1 `shiftL` 32) .|. pad w2

-- | Split a Word64 into two Word32s.
-- Big Endian - the first word of the result is the upper 32 bits.
splitWord64 :: Word64 -> (Word32, Word32)
splitWord64 w =
    let upperMask :: Word64
        upperMask = 0xFFFFFFFF00000000

        lowerMask :: Word64
        lowerMask = 0x00000000FFFFFFFF

     in (fromIntegral $ (w .&. upperMask) `shiftR` 32, fromIntegral $ w .&. lowerMask)
