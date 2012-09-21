
module Data.LFSR.Bits (
  unsignedShiftR
  ) where

import Data.Bits (Bits, shiftR, shiftL, complement, (.&.),
                  isSigned, bitSize)

allSet :: Bits a => a
allSet =  (-1)

unsignedShiftR :: Bits a => a -> Int -> a
unsignedShiftR x i
  | not $ isSigned x = aShift
  | otherwise        = uShift
  where aShift  = shiftR x i
        genMask = complement . shiftL allSet . (bitSize x -) $ i
        uShift  = aShift .&. genMask
