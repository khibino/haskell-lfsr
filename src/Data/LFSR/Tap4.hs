
module Data.LFSR.Tap4 (
  Tap4(..), tap4,
  ) where

import Data.Array (Array, array, (!))


data Tap4 = Tap4 { width :: Int
                 , bits  :: (Int, Int, Int, Int)
                 } deriving Show

tapPair :: (Int, (Int, Int, Int, Int)) -> (Int, Tap4)
tapPair (i, bs) =  (i, Tap4 { width = i, bits = bs })

tap4Table :: Array Int Tap4
tap4Table =
  array (5, 768)
  . map tapPair
  $ [ (8,  (8,  6,  5,  4))
    -- , (16, (16, 14, 13, 11))
    , (16, (16, 15, 13, 4))
    -- , (32, (32, 30, 26, 25))
    , (32, (32, 22, 2, 1))
    , (64, (64, 63, 61, 60))

    -- (128, (128, 127, 126, 121))
    -- (128, (128, 126, 101, 99))
    ]

tap4 :: Int -> Tap4
tap4 =  (tap4Table !)
