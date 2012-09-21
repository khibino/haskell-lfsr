{-# LANGUAGE FlexibleInstances #-}

module Data.LFSR.Tap (
  Tap (..),

  bitsOfTap,

  next, list,

  tap2, tap4
  ) where

import Data.List (foldl', sort, group)
import Data.Array (Array, array, (!))

import Data.Bits (Bits, complementBit, xor, (.&.))

import Data.LFSR.Bits (unsignedShiftR)


class TapBits a where
  bits  :: a -> [Int]

data Tap a = Tap { width :: !Int
                 , tapBits :: !a
                 } deriving Show

tapOfPair :: TapBits t => (Int, t) -> (Int, Tap t)
tapOfPair (i, bs) =  (i, Tap { width = i, tapBits = bs })

bitsOfTap :: (TapBits t, Bits a) => Tap t -> a
bitsOfTap =  foldl' complementBit 0 . map pred . bits . tapBits

next :: Bits a => a -> a -> a
next tBits lfsr =
  (lfsr `unsignedShiftR` 1)
  `xor`
  negate (lfsr .&. 1) .&. tBits

list :: (TapBits t, Bits a) => Tap t -> a -> [a]
list tap iv = rec  where
  rec = iv : map (next $ bitsOfTap tap) rec


type TwoBits  = (Int, Int)
type Tap2 = Tap TwoBits

tap2Table :: Array Int Tap2
tap2Table =
  array (2, 768)
  . map tapOfPair
  $ [
    (2, (2, 1)),
    (9, (9, 5)),
    (17, (17, 14)),
    (33, (33, 20)),
    (65, (65, 47))
    ]

tap2 :: Int -> Tap2
tap2 =  (tap2Table !)


type FourBits = (Int, Int, Int, Int)
type Tap4 = Tap FourBits

tap4Table :: Array Int Tap4
tap4Table =
  array (5, 768)
  . map tapOfPair
  $ [
    (8,  (8,  6,  5,  4)),
    -- (16, (16, 14, 13, 11)),
    (16, (16, 15, 13, 4)),
    -- (32, (32, 30, 26, 25)),
    (32, (32, 22, 2, 1)),
    (64, (64, 63, 61, 60)),

    -- (128, (128, 127, 126, 121))
    (128, (128, 126, 101, 99))
    ]

tap4 :: Int -> Tap4
tap4 =  (tap4Table !)


instance TapBits TwoBits where
  bits (p, q) = [p, q]

instance TapBits FourBits where
  bits (p, q, r, s) = [p, q, r, s]


_testTap :: (TapBits t, Bits a, Ord a) => a -> Tap t -> Bool
_testTap iv = null . filter (/= 1) . map length . group . sort
           . (take (2 * 1024 * 1024))  . (`list` iv)

-- _testTap 10797677
