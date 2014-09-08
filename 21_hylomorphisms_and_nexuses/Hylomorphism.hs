module Hylomorphism where

import Data.Char (ord)

-- Folds, unfolds and hylomorphism

data Tree a = Leaf a | Node [Tree a] deriving Show

fold :: (Either a [b] -> b) -> Tree a -> b
fold f t = case t of
               Leaf x -> f (Left x)
               Node ts -> f (Right (map (fold f) ts))

unfold :: (b -> Either a [b]) -> b -> Tree a
unfold g x = case g x of
                 Left y -> Leaf y
                 Right xs -> Node (map (unfold g) xs)

-- hylo f g = fold f . unfold g

hylo :: (Either a [b] -> b) -> (b -> Either a [b]) -> b -> b
hylo f g x = case g x of
                 Left y -> f (Left y)
                 Right xs -> f (Right (map (hylo f g) xs))

fold2 :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold2 f _g (Leaf x) = f x
fold2 f g (Node ts) = g (map (fold2 f g) ts)

unfold2 :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold2 p v h x = if p x then Leaf (v x) else Node (map (unfold2 p v h) (h x))

hylo2 :: (a -> b) -> ([b] -> b) -> (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> b
hylo2 f g p v h x = if p x then f (v x) else g (map (hylo2 f g p v h) (h x))

-- |
-- (21.1) This function is the deforested version of 'fold f g . unfold p id h'
--
hylo3 :: (a -> a) -> ([a] -> a) -> (a -> Bool) -> (a -> [a]) -> a -> a
hylo3 f g p h x = if p x
        {- base -}    then f x
                      else g (map (hylo3 f g p h) (h x))

data LTree a = LLeaf a | LNode a [LTree a] deriving Show

fill :: (a -> b) -> ([b] -> b) -> Tree a -> LTree b
fill f g = fold2 (lleaf f) (lnode g)

lleaf :: (a -> b) -> a -> LTree b
lleaf f x = LLeaf (f x)

lnode :: ([a] -> a) -> [LTree a] -> LTree a
lnode g ts = LNode (g (map label ts)) ts

label :: LTree a -> a
label (LLeaf x) = x
label (LNode x _ts) = x

exampleTree :: Tree Char
exampleTree = Node [Leaf 'a', Leaf 'b' , Leaf 'c', Node [Leaf 'd', Leaf 'e']]

exampleLTree :: LTree Int
exampleLTree = fill ord sum exampleTree

-- hylo = label . fill f g . unfold p id h

-- Suppose that the tree unfold p id h is a genuine nexus, and suppose we
-- can apply fill f g to it without destroying sharing. Then hylo can be
-- computed more efficiently than by the recursive method of (21.1).

-- In all the examples to come we are going to restrict (21.1) to the case
-- where x is a nonempty list and p is the test for a singleton list.

hylo4 :: ([a] -> b) -> ([b] -> b) -> ([a] -> [[a]]) -> [a] -> b
hylo4 f g h = fold2 f g . mkTree h 

mkTree :: ([a] -> [[a]]) -> [a] -> Tree [a]
mkTree h = unfold2 single id h

single :: [a] -> Bool
single = (1 ==) . length

-- Three examples

-- #1

split :: [a] -> [[a]]
split xs = [take n xs, drop n xs]
  where
    n = length xs `div` 2

-- #2

isegs :: [a] -> [[a]]
isegs xs = [init xs, tail xs]

-- |
-- prop> recover . isegs = id
--
recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss

-- > sum i, i = 1, i = n
-- n(n+1)/2

-- #3

minors :: [a] -> [[a]]
minors [x,y] = [[x],[y]]
minors (x:xs) = map (x:) (minors xs) ++ [xs]
minors _ = error "unexpected error"

-- recurrence relation
--
-- Tree size = S(n)
-- S(0) = 0 /\ S(n + 1) = 1 + (n + 1)S(n)
--
-- S(n + 1) = 1 + (n + 1) * S(n)
-- S(n + 1) = nS(n) + S(n) + 1
-- 
-- S(n) = 1 + nS(n-1)
-- S(n) = nS(n-1) + 1
-- S(n) = n((n-1)S(n-2) + 1) + 1
-- S(n) = n((n-1)((n-2)S(n-3) + 1) + 1) + 1
-- S(n) = n((n-1)(n-2)S(n-3) + (n-1) + 1) + 1
-- S(n) = n(n-1)(n-2)S(n-3) + n(n-1) + n + 1
-- S(n) = n(n-1)(n-2)..0 + n(n-1)(n-2)..2 + n(n-1)(n-2)..3 + .. + n + 1
--
-- n = 1, S(n) = 1
-- n = 2, S(n) = n + 1 = 2 + 1 = 3
-- n = 3, S(n) = n(n-1) + n + 1 = 3(3-1) + 3 + 1 = 6 + 3 + 1 = 10
-- n = 4, S(n) = n(n-1)(n-2) + n(n-1) + n + 1 = 4*3*2 + 4*3 + 4 + 1 = 24 + 12 + 4 + 1 = 41
--
-- sum n!/k!, k = 1 to n

-- Nexus size = (sum bin(n,k), k = 1 to n) = 2^n - 1

example3 :: LTree [Char]
example3 = fill id recover $ mkTree minors "abcde"

