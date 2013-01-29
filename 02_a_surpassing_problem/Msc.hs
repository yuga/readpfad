module Msc where

msc1 :: Ord a => [a] -> Int
msc1 [] = 0
msc1 xs = maximum [scount1 z zs | z:zs <- tails1 xs]

scount1 :: Ord a => a -> [a] -> Int
scount1 x xs = length (filter (x <) xs)

tails1 :: [a] -> [[a]]
tails1 []         = []
tails1 xs@(_:xs') = xs : tails1 xs'

-----------

msc2 :: Ord a => [a] -> Int
msc2 [] = 0
msc2 xs = maximum $ map snd $ table2 xs

table2 :: Ord a => [a] -> [(a,Int)]
table2 [x] = [(x,0)]
table2 xs  = join2 (table2 ys) (table2 zs)
  where
    m       = length xs
    n       = m `div` 2
    (ys,zs) = splitAt n xs

join2 :: Ord a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
join2 txs []  = txs
join2 []  tys = tys
join2 txs tys = [(z, c + tcount2 z tys) | (z,c) <- txs] ++ tys

tcount2 :: Ord a => a -> [(a,Int)] -> Int
tcount2 z tys = scount1 z (map fst tys)

-----------

msc3 :: Ord a => [a] -> Int
msc3 [] = 0
msc3 xs = maximum $ map snd $ table3 xs

table3 :: Ord a => [a] -> [(a,Int)]
table3 [x] = [(x,0)]
table3 xs  = join3 (m - n) (table3 ys) (table3 zs)
  where
    m       = length xs
    n       = m `div` 2
    (ys,zs) = splitAt n xs

join3 :: Ord a => Int -> [(a,Int)] -> [(a,Int)] -> [(a,Int)]
join3 0 txs []  = txs
join3 _ []  tys = tys
join3 n txs@((x,c):txs') tys@((y,d):tys')
  | x <  y = (x,c+n) : join3 n txs' tys
  | x >= y = (y,d) : join3 (n-1) txs tys'

