module HashFunctions where

import Data.Bits
import Data.String

import Types

hash :: HashFun
hash x
  = op x''
  where
    op x = xor (shiftR x 16) x
    x'  = op x * 73244475
    x'' = op x' * 73244475


countOnes :: Int -> Int
countOnes dec
  | dec == 0             = 0
  | dec .&. (bit 0) == 1 = (1 + countOnes (shiftR dec 1))
  | otherwise            = countOnes (shiftR dec 1)


countOnesFrom :: Int -> Int -> Int
countOnesFrom index dec
  = countOnes dec2
  where
    mask = (bit index - 1)
    dec2 = dec .&. mask

getIndex :: Int -> Int ->Int -> Int
getIndex dec batch size
  = shiftR (maskShifted .&. dec) shiftSize
  where
    mask = (bit size -1)
    maskShifted = shiftL mask shiftSize
    shiftSize = batch * size

-- PRE: n is less than length of list
replace :: Int -> [a] -> a -> [a]
replace n list replacement
  = first ++ (replacement : (drop 1 second))
  where
    (first, second) = splitAt (n) list

insertAt :: Int -> a -> [a] -> [a]
insertAt n insertion list
  = first  ++ (insertion : second)
  where
    (first, second) = splitAt (n) list
