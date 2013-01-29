module Minfree where

import Data.Array     (Array, accumArray, assocs, elems)
import Data.Array.ST  (runSTArray, newArray, writeArray)
import Data.List      (partition)

minfree1A :: [Int] -> Int
minfree1A = search . checklistA

minfree1B :: [Int] -> Int
minfree1B = search . checklistB

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklistA :: [Int] -> Array Int Bool
checklistA xs =
    accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
  where
    n = length xs

checklistB :: [Int] -> Array Int Bool
checklistB xs = runSTArray $ do
    a <- newArray (0, n) False
    sequence [writeArray a x True | x <- xs, x <= n]
    return a
  where
    n = length xs

--------

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
  where
    n = length xs

sort :: [Int] -> [Int]
sort xs = concat [replicate k x | (x, k) <- assocs $ countlist xs]

--------

minfree2 :: [Int] -> Int
minfree2 xs = minfrom 0 (length xs, xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs) | n == 0     = a
                  | m == b - a = minfrom b (n - m, vs)
                  | otherwise  = minfrom a (m, us)
  where
    (us, vs) = partition (< b) xs
    b = a + 1 + n `div` 2
    m = length  us
