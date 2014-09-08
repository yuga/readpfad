module Supravel where

supravel :: Ord a => [a] -> [[a]]
supravel = foldr insert []

insert :: Ord a => a -> [[a]] -> [[a]]
insert x []       = [[x]]
insert x (xs:xss) = if x < head xs
                      then (x:xs):xss
                      else xs : insert x xss 

