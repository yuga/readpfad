{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

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

--hylo :: (Either a [b] -> b) -> (x -> Either a [x]) -> x -> b
hylo :: (Either a [b] -> b) -> (b -> Either a [b]) -> b -> b
hylo f g = fold f . unfold g

hylo1 :: (Either a [b] -> b) -> (b -> Either a [b]) -> b -> b
hylo1 f g x = case g x of
                 Left y -> f (Left y)
                 Right xs -> f (Right (map (hylo1 f g) xs))

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

-- annotation
--
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

-- > hylo4 f g h = fold2 f g . mkTree h 

-- #1 'h = split'

split :: [a] -> [[a]]
split xs = [take n xs, drop n xs]
  where
    n = length xs `div` 2

merge :: Ord a => [[a]] -> [a]
merge [ass, []] = ass
merge [[], bss] = bss
merge [ass@(a:as),bss@(b:bs)]
    | a <= b = a : merge [as,bss]
    | otherwise = b : merge [ass,bs]

sort1 :: Ord a => [a] -> [a]
sort1 = hylo4 id merge split

-- #2 'h = isegs'

isegs :: [a] -> [[a]]
isegs xs = [init xs, tail xs]

-- |
-- prop> recover . isegs = id
--
recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss

-- > sum i, i = 1, i = n
-- n(n+1)/2

-- #3 'h = minors'

minors :: [a] -> [[a]]
minors [x,y] = [[x],[y]]
minors (x:xs) = map (x:) (minors xs) ++ [xs]

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

-- Building Nexus

-- Case "h = split"

type Layer1 a = [a]

mkNexus1 :: ([a] -> b) -> ([b] -> b) -> [a] -> b
mkNexus1 f g = label . extractL1 . until singleL1 (stepL1 g) . initialL1 f

initialL1 :: ([a] -> b) -> [a] -> Layer1 (LTree b)
initialL1 f = map (lleaf f . wrap)

wrap :: a -> [a]
wrap x = [x]

stepL1 :: ([b] -> b) -> Layer1 (LTree b) -> Layer1 (LTree b)
stepL1 g = map (lnode g) . group1
    
group1 :: [a] -> [[a]]
group1 [] = []
group1 (x:y:xs) = [x,y] : group1 xs

singleL1 :: Layer1 (LTree b) -> Bool
singleL1 = single

extractL1 :: Layer1 (LTree b) -> LTree b
extractL1 = head

-- |
-- merge sort can work when the size of given list is 2^x.
sort2 :: Ord a => [a] -> [a]
sort2 = mkNexus1 id merge

-- Case "h = isegs"
--
-- group is the only function that is different from the case 'h = split'

type Layer2 a = [a]

mkNexus2 :: ([a] -> b) -> ([b] -> b) -> [a] -> b
mkNexus2 f g = label . extractL2 . until singleL2 (stepL2 g) . initialL2 f

initialL2 :: ([a] -> b) -> [a] -> Layer2 (LTree b)
initialL2 f = map (lleaf f . wrap)

stepL2 :: ([b] -> b) -> Layer2 (LTree b) -> Layer2 (LTree b)
stepL2 g = map (lnode g) . group2
    
group2 :: [a] -> [[a]]
group2 [] = []
group2 (x:y:xs) = [x,y] : group2 (y:xs)

singleL2 :: Layer2 (LTree b) -> Bool
singleL2 = single

extractL2 :: Layer2 (LTree b) -> LTree b
extractL2 = head

-- Case "h = minors"

-- group :: [a] -> [[a]]
-- group [] = []
-- group (x:xs) = map (bind x) xs ++ group xs
--   where bind x y = [x,y]

-- forest
--   n   -- length
--   d   -- depth
--   ts  -- trees
--
-- forest of which length is n and depth is d + 1:
--   forest
--     n
--     d+1
--     [tree_1, tree_2, ..., tree_n]
--        forest  forest       forest
--          n       n-1          1
--          d       d            d
--          ts      ts           ts
--
-- layer_top
--   forest
--     1
--     n-1
--     ts
-- ...
-- layer_2
--   forest
--     n-1
--     1
--     ts
-- layer_bottom
--   forest
--     n
--     0
--     ts

type Layer3 a = [Tree a]

-- |
-- prop> mkNexus3 f g = label . fill f g . mkTree minors
--
mkNexus3 :: ([a] -> b) -> ([b] -> b) -> [a] -> b
mkNexus3 f g = label . extractL3 . until singleL3 (stepL3 g) . initialL3 f

initialL3 :: ([a] -> b) -> [a] -> Layer3 (LTree b)
initialL3 f = map (Leaf . lleaf f . wrap)

stepL3 :: ([b] -> b) -> Layer3 (LTree b) -> Layer3 (LTree b)
stepL3 g = map (mapTree (lnode g)) . group3

group3 :: [Tree a] -> [Tree [a]] 
group3 [_t] = []
group3 (Leaf x:vs) = Node [Leaf [x,y] | Leaf y <- vs] : group3 vs
group3 (Node us:vs) = Node (zipWith combine (group3 us) vs) : group3 vs
    
combine :: Tree [a] -> Tree a -> Tree [a]
combine (Leaf xs) (Leaf x) = Leaf (xs ++ [x])
combine (Node us) (Node vs) = Node (zipWith combine us vs)

mapTree :: ([LTree a] -> LTree a) -> Tree [LTree a] -> Tree (LTree a)
mapTree f (Leaf lts) = Leaf (f lts)
mapTree f (Node ts) = Node (map (mapTree f) ts)

singleL3 :: Layer3 (LTree b) -> Bool
singleL3 = single

extractL3 :: Layer3 (LTree b) -> LTree b
extractL3 = extract . head
  where
    extract (Leaf x) = x
    extract (Node [t]) = extract t

-- Why build the nexus?

-- #1 Take the case h = isegs and consider solve, where

-- |
-- This implements the hylomorphism 'hylo f g minors' without building a nexus
--
solve1 :: ([a] -> b) -> ([b] -> b) -> [a] -> b
solve1 f g = head . until single (map g . group1) . map (f . wrap)

-- |
-- This also implements the hylomorphism 'hylo f g minors' without building a nexus
--
solve2 :: ([a] -> LTree b) -> ([LTree b] -> LTree b) -> [a] -> LTree b
solve2 f g = extractL3 . until singleL3 (step g) . map (Leaf . f . wrap)
  where
    step g' = map (mapTree g') . group3

uncats :: [a] -> [([a], [a])]
uncats [x,y] = [([x],[y])]
uncats (x:xs) = ([x],xs) : map (cons x) (uncats xs)
  where
    cons x' (ys,zs) = (x':ys, zs)

lnode1 :: ([(a,a)] -> a) -> [LTree a] -> LTree a
lnode1 g [u,v] = LNode (g (zip (lspine u) (rspine v))) [u,v]

lspine :: LTree a -> [a]
lspine (LLeaf x) = [x]
lspine (LNode x [u,_v]) = lspine u ++ [x]

rspine :: LTree a -> [a]
rspine (LLeaf x) = [x]
rspine (LNode x [_u,v]) = [x] ++ rspine v

-- #2

unmerges :: [a] -> [([a],[a])]
unmerges [x,y] = [([x],[y])]
unmerges (x:xs) = [([x],xs)] ++ concatMap (add x) (unmerges xs)
  where
    add x' (ys,zs) = [(x':ys, zs), (ys,x':zs)]

traverse :: [LTree a] -> [a]
traverse [] = []
traverse ts = map label ts ++ traverse (concatMap subtrees ts)

subtrees :: LTree a -> [LTree a]
subtrees (LLeaf _x) = []
subtrees (LNode _x ts) = ts

forest :: Int -> [LTree a] -> [LTree a]
forest _k (LLeaf x : ts) = LLeaf x:ts
forest k (LNode x us : vs) = LNode x (forest k (drop k us)) : forest (k+1) vs

lnode2 :: ([(a, a)] -> a) -> [LTree a] -> LTree a
lnode2 g ts = LNode (g (zip xs (reverse ys))) ts
  where
    (xs,ys) = halve (traverse (forest 0 ts))
    halve xs' = splitAt (length xs' `div` 2) xs'

