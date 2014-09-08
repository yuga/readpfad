module Smallest where

import Data.Array

smallest1 :: (Ord a) => Int -> ([a],[a]) -> a
smallest1 k (xs,ys) = union1 (xs,ys) !! k

union1 :: (Ord a) => ([a],[a]) -> [a]
union1 (xs,[]) = xs
union1 ([],ys) = ys
union1 (x:xs,y:ys)
    | x < y = x : union1 (xs,y:ys)
    | x > y = y : union1 (x:xs,ys) -- no "==" exists because merging adjoint lists

--------

smallest2 :: (Ord a) => Int -> ([a],[a]) -> a
smallest2 k ([],ws) = ws !! k
smallest2 k (zs,[]) = zs !! k
smallest2 k (zs,ws) =
  case (a < b, k <= p+q) of
    (True,True)   -> smallest2 k (zs,us)
    (True,False)  -> smallest2 (k-p-1) (ys,ws)
    (False,True)  -> smallest2 k (xs,ws)
    (False,False) -> smallest2 (k-q-1) (zs,vs)
  where
    p         = (length zs) `div` 2
    q         = (length ws) `div` 2
    (xs,a:ys) = splitAt p zs
    (us,b:vs) = splitAt q ws

--------

smallest3 :: (Ord a) => Int -> (Array Int a, Array Int a) -> a
smallest3 k (xa,ya) = search3 k (0,m+1) (0,n+1)
  where
    (0,m) = bounds xa
    (0,n) = bounds ya
    search3 k (lx,rx) (ly,ry)
      | lx == rx  = ya!(k+ly)
      | ly == ry  = xa!(k+lx)
      | otherwise = case (xa!mx < ya!my, k <= (mx-lx)+(my-ly)) of
          (True,True)   -> search3 k (lx,rx) (ly,my)
          (True,False)  -> search3 (k-(mx-lx)-1) (mx+1,rx) (ly,ry)
          (False,True)  -> search3 k (lx,mx) (ly,ry)
          (False,False) -> search3 (k-(my-ly)-1) (lx,rx) (my+1,ry)
      where
        mx = (lx+rx) `div` 2
        my = (ly+ry) `div` 2

smallest3' :: (Ord a) => Int -> ([a],[a]) -> a
smallest3' k (xs,ys) = smallest3 k (xa,ya)
  where
    xa = listArray (0,length xs - 1) xs
    ya = listArray (0,length ys - 1) ys
