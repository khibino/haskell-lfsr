
module Data.LFSR.Tap2 (
  Tap2(..), tap2,
  ) where

import Data.Array (Array, array, (!))


data Tap2 = Tap2 { width :: Int
                 , bits  :: (Int, Int)
                 } deriving Show

tapPair :: (Int, (Int, Int)) -> (Int, Tap2)
tapPair (i, bs) =  (i, Tap2 { width = i, bits = bs })

tap2Table :: Array Int Tap2
tap2Table =
  array (2, 768)
  . map tapPair
  $ [ (7,  (7, 6))
    , (15, (15, 14))
    , (31, (31, 28))
    , (63, (63, 62))
    ]

tap2 :: Int -> Tap2
tap2 =  (tap2Table !)
