{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array    (Array, array, (!))
import Data.List     (sort, sortBy)

infixl 6 <+>, <->

type A = Int

(<+>) :: A -> A -> A
(<+>) = (+)

(<->) :: A -> A -> A
(<->) = (-)

type Label a = (a,(Int,Int))

--------

sortsums1 :: [A] -> [A] -> [A]
sortsums1 xs ys = sort [x <+> y | x <- xs, y <- ys]

--------

sortsums2 :: [A] -> [A] -> [A]
sortsums2 xs ys = map fst (sortsubs2 xs (map negate ys))

sortsubs2 :: [A] -> [A] -> [Label A]
sortsubs2 xs ys = sort (subs2 xs ys)

subs2 :: [A] -> [A] -> [Label A]
subs2 xs ys = [(x<->y,(i,j)) | (x,i) <- zip xs [1..], (y,j) <- zip ys [1..]] 

--------

sortsums3 :: [A] -> [A] -> [A]
sortsums3 xs ys = map fst (sortsubs3 xs (map negate ys))

sortsubs3 :: [A] -> [A] -> [Label A]
sortsubs3 xs ys = sortBy (cmp (mkArray3 xs ys)) (subs2 xs ys)

cmp :: Array (Int,Int,Int) A -> Label A -> Label A -> Ordering
cmp a (x,(i,j)) (y,(k,l)) = compare (a!(1,i,k)) (a!(2,j,l))

mkArray3 :: [A] -> [A] -> Array (Int,Int,Int) A
mkArray3 xs ys = array b (zip (table3 xs ys) [1..])
  where
    b = ((1,1,1),(2,p,p))
    p = max (length xs) (length ys)

table3 :: [A] -> [A] -> [(Int,Int,Int)]
table3 xs ys = map snd (map (tag3 1) xxs `merge` map (tag3 2) yys)
  where
    xxs = sortsubsp3 xs
    yys = sortsubsp3 ys
    merge = (++)

tag3 :: Int -> Label A -> (A,(Int,Int,Int))
tag3 i (x,(j,k)) = (x,(i,j,k))

sortsubsp3 :: [A] -> [A] -> [Label A]
sortsubsp3 []  = []
sortsubsp3 [w] = [(w<->w,(1,1))]
sortsubsp3 ws  = foldr1 merge [xxs,
                               map (incr m) xys,
                               map (incl m) yxs,
                               map (incb m) yys]
  where
    merge   = (++)
    xxs     = sortsubsp3 xs
    xys     = sortBy (cmp (mkArray3 xs ys)) (subs2 xs ys)
    yxs     = map switch (reverse xys)
    yys     = sortsubsp3 ys
    (xs,ys) = splitAt m ws
    m       = length ws `div` 2

incl m (x,(i,j)) = (x,(m+i,j))
incr m (x,(i,j)) = (x,(i,m+j))
incb m (x,(i,j)) = (x,(m+i,m+j))
switch (x,(i,j)) = (negate x,(j,i))

--------


