{-# LANGUAGE BangPatterns #-}

module Loopless where

import Prelude hiding (concat, flip, id, reverse)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr step b = case step b of
                     Just (a, b') -> a : unfoldr step b'
                     Nothing      -> []

-- --|
-- -- step      - constant time
-- -- prolog x  - O(n) steps
-- unfoldr step . prolog

id :: [a] -> [a]
id = unfoldr uncons . prolog

prolog :: [a] -> [a]
prolog = id

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x,xs)

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

reverse' :: [a] -> [a]
reverse' = unfoldr uncons . foldl (flip (:)) []

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

concat :: [[a]] -> [a]
concat = unfoldr step . filter (not . null)

step :: [[a]] -> Maybe (a,[[a]])
step [] = Nothing
step ((x:xs):xss) = Just (x,consList xs xss)

consList :: [a] -> [[a]] -> [[a]]
consList xs xss = if null xs then xss else xs : xss

type Forest a = [Rose a]
data Rose a = Node a (Forest a) deriving Show

preorder :: Forest a -> [a]
preorder [] = []
preorder (Node x xs : ys) = x : preorder (xs ++ ys)

step2 :: [Forest a] -> Maybe (a, [Forest a])
step2 [] = Nothing
step2 ((Node x xs : ys) : zss) = Just (x, consList xs (consList ys zss))

preorder' :: Forest a -> [a]
preorder' = unfoldr step2 . wrapList
  where
    wrapList xs = consList xs []

box :: [a] -> [a] -> [a]
[] `box` ys = ys
(x:xs) `box` ys = ys ++ [x] ++ (xs `box` reverse ys)

box' :: [a] -> [a] -> [a]
xs `box'` ys = mix xs (ys, reverse ys)

mix :: [a] -> ([a], [a]) -> [a]
mix [] (ys, _sy) = ys
mix (x:xs) (ys, sy) = ys ++ [x] ++ mix xs (sy,ys)

-- the proof of associativity of `box`:
--
-- (x:xs) `box` [] = [] ++ [x] ++ (xs `box` [])
-- [] `box` ys = ys
--
--   ((x:xs) `box` (y:ys)) `box` zs
-- = ((y:ys) ++ [x] ++ (xs `box` reverse (y:ys))) `box` zs
-- = zs ++ [y] ++ ((ys ++ [x] ++ (xs `box` reverse (y:ys))) `box` reverse zs)
--
--   (x:xs) `box` ((y:ys) `box` zs)
-- = (x:xs) `box` (zs ++ [y] ++ (ys `box` reverse zs))
-- = (zs ++ [y] ++ (ys `box` reverse zs)) ++ [x] ++ (xs `box` reverse (zs ++ [y] ++ (ys `box` reverse zs)))
--
-- to complete above proof, the below subsidiary identities:
--
--   (xs + [y] + ys) `box` zs = (xs `box` zs) + [y] + (ys `box` zs')
--   reverse (xs `box` ys) = (reverse xs) `box` ys'
--
--   zs' = if even (length xs) then reverse zs else zs
--   ys' = if even (length xs) then reverse ys else ys

boxall :: [[a]] -> [a]
boxall = foldr box []

-- boxall (xs:xss) = xs `box` (boxall xss) = xs `box` (reverse (boxall xss))

--   reverse (xs `box` ys) = xs `xob` (reverse ys)
-- since
--   reverse (reverse xs) = xs,
-- so,
--   xs `xob` sy = reverse (xs `box` (reverse sy))
-- or,
--   [] `xob` sy = sy
--   (x:xs) `xob` sy = (xs `xob` reverse sy) ++ [x] ++ sy
--
-- xs `xob` sy = if even (length xs) then (reverse xs) `box` (reverse sy)
--                                   else (reverse xs) `box` sy


op1 :: [a] -> ([a], [a]) -> ([a], [a])
op1 [] (ys, sy) = (ys, sy)
op1 (x:xs) (ys, sy) =
    (ys ++ [x] ++ zs, sz ++ [x] ++ sy)
  where
    (zs, sz) = op1 xs (sy, ys)

op2 :: [a] -> ([a], [a]) -> ([a], [a])
op2 xs (ys, sy) = if even (length xs)
                      then (mix xs (ys, sy), mix (reverse xs) (sy, ys))
                      else (mix xs (ys, sy), mix (reverse xs) (ys, sy))

boxall1 :: [[a]] -> [a]
boxall1 = preorder' . fst . foldr op1' ([], [])

boxall2 :: [[a]] -> [a]
boxall2 = preorder' . fst . foldr op2' ([], [])

op1' :: [a] -> (Forest a, Forest a) -> (Forest a, Forest a)
op1' [] (ys, sy) = (ys, sy)
op1' (x:xs) (ys, sy) =
    (ys ++ [Node x zs], sz ++ [Node x sy])
  where
    (zs, sz) = op1' xs (sy, ys)

op2' :: [a] -> (Forest a, Forest a) -> (Forest a, Forest a)
op2' xs (ys, sy) =
    if even (length xs)
        then (mix2 xs (ys, sy), mix2 (reverse xs) (sy, ys))
        else (mix2 xs (ys, sy), mix2 (reverse xs) (ys, sy))

mix2 :: [a] -> (Forest a, Forest a) -> Forest a
mix2 [] (ys, _sy) = ys
mix2 (x:xs) (ys, sy) = ys ++ [Node x (mix2 xs (sy,ys))]

figure1, figure2 :: (Forest Int, Forest Int)
figure1 = foldr op1' ([],[]) [[1,2],[3,4]]
figure2 = foldr op2' ([],[]) [[1,2],[3,4]]

data Queue a = Queue [a] !(List a) [a]
             deriving (Show)

data List a = Cons a !(List a)
            | Nil
            deriving (Show)

rotate :: [a] -> List a -> [a] -> [a]
rotate [] (Cons r _) ss = r:ss
rotate (f:fs') (Cons r rs') ss = (f : rotate fs' rs' (r:ss)) 
rotate _ Nil _ = error "unexpected error: invariant is broken, don't have to reach here"

exec :: Queue a -> Queue a
exec (Queue fs rs (_:ss')) = Queue fs rs ss'
exec (Queue fs rs []) = Queue f' Nil f'
  where
    f' = rotate fs rs []

insert :: Queue a -> a -> Queue a
insert (Queue fs rs ss) a =
    let !q = exec (Queue fs (Cons a rs) ss)
    in q

remove :: Queue a -> (a, Queue a)
remove (Queue (f:fs') rs ss) =
    let !q = exec (Queue fs' rs ss)
    in (f, q)
remove _ = error "fail to remove: empty"

empty :: Queue a
empty = Queue [] Nil []

isempty :: Queue a -> Bool
isempty (Queue [] _ _) = True
isempty _ = False

type Forest2 a = Queue (Rose2 a)
data Rose2 a = Node2 a (Forest2 a)

boxall3 :: [[a]] -> [a]
boxall3 = unfoldr step3 . wrapQueue . fst . foldr op3' (empty, empty)

boxall4 :: [[a]] -> [a]
boxall4 = unfoldr step3 . wrapQueue . fst . foldr op4' (empty, empty)

step3 :: [Forest2 a] -> Maybe (a, [Forest2 a])
step3 [] = Nothing
step3 (zs:zss) = Just (x, consQueue xs (consQueue ys zss))
  where
    (Node2 x xs, ys) = remove zs

consQueue :: Queue a -> [Queue a] -> [Queue a]
consQueue xs xss = if isempty xs then xss else xs:xss

wrapQueue :: Queue a -> [Queue a]
wrapQueue xs = consQueue xs []

op3' :: [a] -> (Forest2 a, Forest2 a) -> (Forest2 a, Forest2 a)
op3' [] (ys, sy) = (ys, sy)
op3' (x:xs) (ys, sy) =
    (insert ys (Node2 x zs), insert sz (Node2 x sy))
  where
    (zs, sz) = op3' xs (sy, ys)

op4' :: [a] -> (Forest2 a, Forest2 a) -> (Forest2 a, Forest2 a)
op4' xs (ys, sy) =
    if even (length xs)
        then (mix3 xs (ys, sy), mix3 (reverse xs) (sy, ys))
        else (mix3 xs (ys, sy), mix3 (reverse xs) (ys, sy))

mix3 :: [a] -> (Forest2 a, Forest2 a) -> Forest2 a
mix3 [] (ys, _sy) = ys
mix3 (x:xs) (ys, sy) = insert ys (Node2 x (mix3 xs (sy,ys)))

