{-# LANGUAGE BangPatterns #-}

module Minfree where

import Control.Monad.ST (ST, runST)
import Data.Array       (Array, accumArray, assocs, elems)
import Data.Array.ST    (STUArray, newArray, newListArray, readArray, runSTArray, writeArray)
import Data.List        (partition)
import Debug.Trace      (trace)

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
    n = maximum xs

sort :: [Int] -> [Int]
sort xs = concat [replicate k x | (x, k) <- assocs $ countlist xs]

--------

minfree2 :: [Int] -> Int
minfree2 xs = minfrom2 0 (length xs, xs)

minfrom2 :: Int -> (Int, [Int]) -> Int
minfrom2 a (n, xs) | n == 0     = a
                   | m == b - a = {- trace ("minfrom3: a=" ++ show a ++ ", n=" ++ show n) $ -} minfrom2 b (n - m, vs)
                   | otherwise  = {- trace ("minfrom3: a=" ++ show a ++ ", n=" ++ show n) $ -} minfrom2 a (m,     us)
  where
    (us, vs) = partition (< b) xs
    b = a + 1 + n `div` 2
    m = length  us

--------

minfree3 :: [Int] -> Int
minfree3 xs = runST $ do
    arr <- newListArray (0,n) xs
    minfrom3 0 (0, n, arr)
  where
    n = length xs

minfrom3 :: Int -> (Int, Int, STUArray s Int Int) -> ST s Int
minfrom3 a (s, n, ua) 
  | n == 0    = return a
  | otherwise = {- trace ("minfrom3: a=" ++ show a ++ ", s=" ++ show s ++ ", n=" ++ show n) $ -} do
      p <- partition3 (< b) s (s+n) ua
      let m = p - s
      if m == b - a then
          minfrom3 b (p, n - m, ua)
       else
          minfrom3 a (s, m,     ua)
  where
    b = a + 1 + n `div` 2

partition3 :: (Int -> Bool) -> Int -> Int -> STUArray s Int Int -> ST s Int
partition3 f s n ua = from_left s n
  where
    --from_left :: Int -> Int -> ST s Int
    from_left i j
      | i == j    = return i
      | otherwise = do
          x <- readArray ua i
          if f x then
              from_left (i+1) j
           else
              from_right i (j-1)

    --from_right :: Int -> Int -> ST s Int
    from_right i j
      | i == j    = return i
      | otherwise = do
          x <- readArray ua j
          if f x then do
              y <- readArray ua i
              writeArray ua i x
              writeArray ua j y
              from_left (i+1) j
           else
              from_right i (j-1)
