{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Mark where

-- Specification

type Node = Int
type Graph = Node -> (Node, Node)

graph :: Graph
graph 1 = (2, 3)
graph 2 = (4, 5)
graph 3 = (5, 6)
graph 4 = (1, 3)
graph 5 = (4, 6)
graph 6 = (1, 2)
graph 7 = (4, 5)
graph 8 = (5, 6)
graph _ = error "unknown node"

left :: Graph -> Node -> Node
left g x = fst (g x)

right :: Graph -> Node -> Node
right g x = snd (g x)

setl :: Graph -> Node -> Node -> Graph
setl g x y = \z -> if z == x then (y, right g z) else g z

setr :: Graph -> Node -> Node -> Graph
setr g x y = \z -> if z == x then (left g z, y) else g z

mark :: Graph -> Node -> (Graph, Node -> Bool)
mark g root = seek0 (g, const False) [root]

seek0 :: (Graph, Node -> Bool) -> [Node] -> (Graph, Node -> Bool)
seek0 (g, m) [] = (g, m)
seek0 (g, m) (x:xs)
    | not (m x) = seek0 (g, set m x) (left g x : right g x : xs)
    | otherwise = seek0 (g, m) xs -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            -- ~~       stack
                            -- stack

-- |
-- call f y recursively until y == x appears or const False is reached.
-- you set a node to create a function, the new one will return True
-- when you apply it to the node.
set :: (Node -> Bool) -> Node -> (Node -> Bool)
set f x = \y -> if y == x then True else f y

-- |
-- unset a node, so that new function will return False for the same node.
unset :: (Node -> Bool) -> Node -> (Node -> Bool)
unset f x = \y -> if y == x then False else f y

-- Safe replacement

replace :: Eq a => (a -> b) -> a -> b -> (a -> b)
replace f x y = \z -> if z == x then y else f z

-- the below identities hold hold if x is not on the list xs
--
-- > map f xs = map (replace f x y) xs
-- > filter p xs = filter (replace p x y) xs

-- Eliminating duplicate entries

-- |
-- prop> clean m xs = all m xs && nodups xs
--
seek1proto :: (Graph, Node -> Bool) -> Node -> [Node] -> (Graph, Node -> Bool)
seek1proto (g, m) x xs = seek0 (g, m) (x : map (right g) xs)

mark1 :: Graph -> Node -> (Graph, Node -> Bool)
mark1 g root = seek1 (g, const False) root []

seek1 :: (Graph, Node -> Bool) -> Node -> [Node] -> (Graph, Node -> Bool)
seek1 (g, m) x xs
    | not (m x) = seek1 (g, set m x) (left g x) (x : xs)
    | null xs   = (g, m)                     -- ~~~~~~~~
    | otherwise = seek1 (g, m) (right g (head xs)) (tail xs)
                                                -- ~~~~~~~~~

-- Threading the stack

--
-- threaded g m p x xs = clean m xs && and [link u v|(u, v) <- zip (x:xs) xs]
--   where link u v = if p v then u == left g v else u == right g v
--
-- if p v = true, then u (= x) == left gv since seek1 (g, set m x) (left g x) (x : xs).
--
seek2proto :: (Graph, Node -> Bool) -> (Node -> Bool) -> Node -> [Node] -> (Graph, Node -> Bool)
seek2proto (g,m) p x xs = seek1 (g, m) x (filter p xs)

-- Below we refer to the following fact with the hint “threadedness”:
-- provided
--     m x and x ∈/ xs, we have
-- threaded g m p x xs ⇒ threaded g m (set p x) (left g x) (x:xs) ∧
--                        threaded g m (unset p x) (right g x) (x : xs)

mark2proto :: Graph -> Node -> (Graph, Node -> Bool)
mark2proto g x = seek2proto (g, const False) (const False) x []

-- Case "not (m x)":
--
--   seek2 (g, m) p x xs
-- =     {- definition -}
--   seek1 (g, m) x (filter p xs)
-- =     {- case assumption not (m x) -}
--   seek1 (g, set m x) (left g x) (x : filter p xs)
-- =     {- safe replacement, since x ∈/ xs -}
--   seek1 (g, set m x) (left g x) (x : filter (set p x) xs)
-- =     {- since set p x x = True -}
--   seek1 (g, set m x) (left g x) (filter (set p x) (x : xs))
-- =     {- definition of seek2, and threaded -}
--   seek2 (g, set m x) (set p x) (left g x) (x : xs)

-- Case "m x":
--
--   find2 (g, m) p xs = seek1 (g, m) x (filter p xs)
--
--   Case "xs = []"
--     find2 (g, m) p [] = (g, m)
--   Case "xs = y:ys && not (p y)"
--     find2 (g, m) p (y:ys) = find2 (g, m) p ys
--   Case "p y"
--     find2 (g, m) p (y:ys)
--   =     {- definition of find2 and seek1 in th case p y -}
--     seek1 (g, m) (right g y) (filter p ys)
--   =     {- safe replacement since y ∈/ ys -}
--     seek1 (g, m) (right g y) (filter (unset p y) ys)
--   =     {- since unset p v v = False -}
--     seek1 (g, m) (right g y) (filter (unset p y) y:ys)
--   =     {- definition of seek2, and threaded -}
--     seek2 (g, m) (unset p y) (right g y) (y : ys)

mark2 :: Graph -> Node -> (Graph, Node -> Bool)
mark2 g root = seek2 (g, const False) (const False) root []

seek2 :: (Graph, Node -> Bool) -> (Node -> Bool) -> Node -> [Node] -> (Graph, Node -> Bool)
seek2 (g, m) p x xs
    | not (m x) = seek2 (g, set m x) (set p x) (left g x) (x : xs)
    | otherwise = find2 (g, m) p xs

find2 :: (Graph, Node -> Bool) -> (Node -> Bool) -> [Node] -> (Graph, Node -> Bool)
find2 (g, m) p [] = (g, m)
find2 (g, m) p (y : ys)
    | not (p y) = find2 (g, m) p ys
    | otherwise = seek2 (g, m) (unset p y) (right g y) (y : ys)

-- Representing the stack by a linked list

stack :: Graph -> (Node -> Bool) -> Node -> [Node]
stack g p x | x == 0    = []
            | p x       = x : stack g p (left g x)
            | not (p x) = x : stack g p (right g x)

-- x : stack g p y = stack (setl g x y) (set p x) x  -- (26.1)

restore :: Graph -> (Node -> Bool) -> Node -> [Node] -> Graph
restore g p x [] = g
restore g p x (y:ys) | p y       = restore (setl g y x) p y ys
                     | not (p y) = restore (setr g y x) p y ys

-- threaded g m p x x xs => restore g p x xs = g

seek3proto :: (Graph, Node -> Bool) -> (Node -> Bool) -> Node -> Node -> (Graph, Node -> Bool)
seek3proto (g, m) p x y = seek2 (restore g p x xs, m) p x xs
  where
    xs = stack g p y

find3proto :: (Graph, Node -> Bool) -> (Node -> Bool) -> Node -> Node -> (Graph, Node -> Bool)
find3proto (g, m) p x y = find2 (restore g p x xs, m) p xs
  where
    xs = stack g p y

-- swing g y x = setr (setl g y x) y (left g y)
--
--   setr (swing g y x) y (right g y)
-- =   {- definition of swing -}
--   setr (setr (setl g y x) y (left g y)) y (right g y)
-- =   {- since the result of inner "setr" is overwritten by outer "setr" -}
--   setl g y x

mark3 :: Graph -> Node -> (Graph, Node -> Bool)
mark3 g root = seek3 (g, const False) (const False) root 0

seek3 :: (Graph, Node -> Bool) -> (Node -> Bool) -> Node -> Node -> (Graph, Node -> Bool)
seek3 (g, m) p x y
    | not (m x) = seek3 (setl g x y, set m x) (set p x) (left g x) x
    | otherwise = find3 (g, m) p x y

find3 :: (Graph, Node -> Bool) -> (Node -> Bool) -> Node -> Node -> (Graph, Node -> Bool)
find3 (g, m) p x y
    | y == 0    = (g, m)
    | p y       = seek3 (swing g y x, m) (unset p y) (right g y) y
    | otherwise = find3 (setr g y x, m) p y (right g y)
  where
    swing g y x = setr (setl g y x) y (left g y)

example :: [Bool]
example = map (snd marked) [1..8]
  where
    marked = mark graph 1

