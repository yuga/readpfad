module Invert where

-- invert :: ((a,a) -> a) -> a -> [(a,a)]
-- invert f z = undefined

---------------------------------------------------------------

-- |
-- The first implementation of invert.
--
-- >>> invert1 (\(x,y) -> 10*x + y) 34
-- [(0,34),(1,24),(2,14),(3,4)] 
--
invert1 :: (Integral a) => ((a,a) -> a) -> a -> [(a,a)]
invert1 f z = [(x,y) | x <- [0..z], y <- [0..z], f (x,y) == z]

---------------------------------------------------------------

invert2 :: (Integral a) => ((a,a) -> a) -> a -> [(a,a)]
invert2 f z = [(x,y) | x <- [0..z], y <- [0..z-x], f (x,y) == z]

---------------------------------------------------------------

invert3 :: (Integral a) => ((a,a) -> a) -> a -> [(a,a)]
invert3 f z = find3 (0,z) f z

find3 :: (Integral a) => (a,a) -> ((a,a) -> a) -> a -> [(a,a)]
find3 (u,v) f z = [(x,y) | x <- [u..z], y <- [v,v-1..0], f (x,y) == z]

---------------------------------------------------------------

-- |
-- The initial version of saddleback search
--
invert4 :: (Integral a) => ((a,a) -> a) -> a -> [(a,a)]
invert4 f z = find4 (0,z) f z

find4 :: (Integral a) => (a,a) -> ((a,a) -> a) -> a -> [(a,a)]
find4 (u,v) f z
    | u > z || v < 0 = []
    | z' <  z        = find4 (u+1,z) f z
    | z' == z        = (u,v) : find4 (u+1,v-1) f z
    | otherwise      = find4 (u,v-1) f z
  where
    z' = f (u,v)

---------------------------------------------------------------

invert5 :: (Integral a) => ((a,a) -> a) -> a -> [(a,a)]
invert5 f z = find5 (0,m) f z
  where
    m = maximum $ filter (\y -> f (0,y) <= z) [0..z]

find5 :: (Integral a) => (a,a) -> ((a,a) -> a) -> a -> [(a,a)]
find5 (u,v) f z
    | u > n || z < 0 = []
    | z' <  z        = find5 (u+1,z) f z
    | z' == z        = (u,v) : find5 (u+1,v-1) f z
    | otherwise      = find5 (u,v-1) f z
  where
    n = maximum $ filter (\x -> f (x,0) <= z) [0..z]
    z' = f (u,v)

---------------------------------------------------------------

bsearch :: (Integral a) => (a -> a) -> (a,a) -> a -> a
bsearch g (a,b) z
    | a+1 == b  = a
    | g m <= z  = bsearch g (m,b) z
    | otherwise = bsearch g (a,m) z
  where
    m = (a+b) `div` 2

---------------------------------------------------------------

-- |
-- Saddleback search with binary search to compute the boundaries
--
invert6 :: (Integral a) => ((a,a) -> a) -> a -> [(a,a)]
invert6 f z = find6 (0,m) f' z
  where
    m         = bsearch (\y -> f (0,y)) (-1,z+1) z
    f' (0,-1) = 0
    f' (-1,0) = 0
    f' t      = f t

find6 :: (Integral a) => (a,a) -> ((a,a) -> a) -> a -> [(a,a)]
find6 (u,v) f z
    | u > n || v < 0 = []
    | z' <  z        = find6 (u+1,z) f z
    | z' == z        = (u,v) : find6 (u+1,v-1) f z
    | otherwise      = find6 (u,v-1) f z
  where
    n = bsearch (\x -> f (x,0)) (-1,z+1) z
    z' = f (u,v)

---------------------------------------------------------------

-- |
-- The final version
--
invert7 :: (Integral a) => ((a,a) -> a) -> a -> [(a,a)]
invert7 f z = find7 (0,m) (n,0) f' z
  where
    m         = bsearch (\y -> f' (0,y)) (-1,z+1) z
    n         = bsearch (\x -> f' (x,0)) (-1,z+1) z
    f' (0,-1) = 0
    f' (-1,0) = 0
    f' t      = f t

find7 :: (Integral a) => (a,a) -> (a,a) -> ((a,a) -> a) -> a -> [(a,a)]
find7 (u,v) (r,s) f z
    | u > r || v < s = []
    | v - s <= r - u = rfind $ bsearch (\x -> f (x,q)) (u-1,r+1) z
    | otherwise      = cfind $ bsearch (\y -> f (p,y)) (s-1,v+1) z
  where
    p       = (u+r) `div` 2
    q       = (v+s) `div` 2
    rfind p = (if f (p,q) == z then
                   (p,q) : find7 (u,v) (p-1,q+1) f z
               else
                   find7 (u,v) (p,q+1) f z)
              ++
              find7 (p+1,q-1) (r,s) f z
    cfind q = find7 (u,v) (p-1,q+1) f z
              ++
              (if f (p,q) == z then
                   (p,q) : find7 (p+1,q-1) (r,s) f z
               else
                   find7 (p+1,q) (r,s) f z)

