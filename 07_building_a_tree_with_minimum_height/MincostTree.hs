module MincostTree where

data Tree = Leaf Int | Fork Tree Tree deriving Show
type Forest = [Tree]

--------

cost1 :: Tree -> Int
cost1 (Leaf x)   = x
cost1 (Fork u v) = 1 + (cost1 u `max` cost1 v)

trees1 :: [Int] -> [Tree]
trees1 [x]    = [Leaf x]
trees1 (x:xs) = concatMap (prefixes1 x) (trees1 xs)

prefixes1 :: Int -> Tree -> [Tree]
prefixes1 x t@(Leaf y)   = [Fork (Leaf x) t]
prefixes1 x t@(Fork u v) = [Fork (Leaf x) t] ++ [Fork u' v | u' <- prefixes1 x u]

--------

foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
foldrn f g [x]    = g x
foldrn f g (x:xs) = f x (foldrn f g xs)

trees2 :: [Int] -> [Tree]
trees2 = foldrn (concatMap . prefixes1) (wrap . Leaf)

wrap :: a -> [a]
wrap x = [x]

--------

trees3 :: [Int] -> [Tree]
trees3 = map rollup3 . forests3

forests3 :: [Int] -> [Forest]
forests3 = foldrn (concatMap . prefixes3) (wrap . wrap . Leaf)

prefixes3 :: Int -> Forest -> [Forest]
prefixes3 x ts = [Leaf x : rollup3 (take k ts) : drop k ts | k <- [1..length ts]]

rollup3 :: Forest -> Tree
rollup3 = foldl1 Fork

mincostTree3 :: [Int] -> Tree
mincostTree3 = minBy3 cost1 . trees3

minBy3 :: (Tree -> Int) -> Forest -> Tree
minBy3 f = foldl1 (cmp3 f)

cmp3 :: (Tree -> Int) -> Tree -> Tree -> Tree
cmp3 f u v = if f u <= f v then u else v

-------

mincostTree4 :: [Int] -> Tree
mincostTree4 = foldl1 Fork . map  snd . foldrn insert (wrap . leaf)

insert :: Int -> [(Int, Tree)] -> [(Int, Tree)]
insert x ts = leaf x : split x ts

split :: Int -> [(Int, Tree)] -> [(Int, Tree)]
split x [u]      = [u]
split x (u:v:ts) = if x `max` fst u < fst v
                     then u:v:ts
                     else split x (fork u v :ts)

leaf :: Int -> (Int, Tree)
leaf x = (x, Leaf x)

fork :: (Int, Tree) -> (Int, Tree) -> (Int, Tree)
fork (a,u) (b,v) = (1 + a `max` b, Fork u v)


