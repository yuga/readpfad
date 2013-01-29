[読書メモ][PFAD]

Pearls of Functional Algorithm Design
 
1 The smallest free number
======================================
 
0からはじまる有限の連続した自然数の集合XがあってXからいくつか少数の
自然数を取り除いた集合Yを作る。
ここで集合Xに存在してYに存在しない一番小さな自然数を探そうというとき、
まず集合Xから集合Yに含まれるすべての要素を削除する集合の差の演算を \\ として、
X \\ Yと書く。これは(\\) X Y = [x|x<-X,y<-Y,x!=y]と直積になるので
この演算のコストはΘ(n^2)である。

この問題を線形時間で解く方法が2つある。

An array-based solution
--------------------------

まず、Data.Arrayライブラリを使う方法。

> import Data.Array     (Array, accumArray, assocs, elems)
> import Data.Array.ST  (runSTArray, newArray, writeArray)
> import Data.List      (partition)
>
> minfree1A :: [Int] -> Int
> minfree1A = search . checklistA
> 
> minfree1B :: [Int] -> Int
> minfree1B = search . checklistB
> 
> search :: Array Int Bool -> Int
> search = length . takeWhile id . elems
> 
> checklistA :: [Int] -> Array Int Bool
> checklistA xs =
>     accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
>   where
>     n = length xs
> 
> checklistB :: [Int] -> Array Int Bool
> checklistB xs = runSTArray $ do
>     a <- newArray (0, n) False
>     sequence [writeArray a x True | x <- xs, x <= n]
>     return a
>   where
>     n = length xs


この手法をつかってソート行うこともできる。

> countlist :: [Int] -> Array Int Int
> countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
>   where
>     n = length xs
> 
> sort :: [Int] -> [Int]
> sort xs = concat [replicate k x | (x, k) <- assocs $ countlist xs]


A devide and conquer solution
------------------------------

つぎに、この問題のsemiring fusionな性質を利用した分割統治法。

半環(semiring)
http://ja.wikipedia.org/wiki/%E5%8D%8A%E7%92%B0
  3. 乗法は加法の上に分配的である:
    a ・(b + c) = (a ・ b) + (a ・ c)
    (a + b)・ c = (a ・ c) + (b ・ c)
 
集合の和の演算子を ++ とする。
集合X = X1 ++ X2とおいて、上記規則をあてはめると
X \\ Y = (X1 ++ X2) \\ Y = (X1 \\ Y) ++ (X2 \\ Y)
となる。
 
さらに、集合Y = Y1 ++ Y2とおいて、Y1をX2と疎、Y2をX1と疎であるとする。
つまりX2 \\ Y1 = X2、X1 \\ Y2 = X1である。
このとき(X1 ++ X2) \\ (Y1 ++ Y2) = (X1 \\ Y1) ++ (X2 \\ Y2)となる。
 
問題はYに存在しないXの要素で一番小さな自然数を探すことなので
X1とX2の分割とY1とY2の分割を、ある数値b1との大小比較で行うとしたら、
X1=[x|x<-X,x<b], X2=[x|x<-X,x>=b]
Y1=[y|y<-Y,y<b], Y2=[y|y<-Y,y>=b]となる。
このときX1の要素のうちY1に含まれないものがある場合|X1|>|Y1|となる。
|X1|>|Y1|のとき求める解はX1のなかにある
(X1|=|Y1|の場合求める解はX2のなかにあることになる)。
そこであらたな集合分割の数値b2を設けて集合X1と集合Y1に対し同様の処理を
再帰的(1,2,...,m)に行っていくと、最後に集合Ymが空になって集合Xmに解が残る。
 
集合の分割をつねに二等分になるようにすると
コストは漸化式T(n)=T(n/2)+Θ(n)なので、T(n)=Θ(n)である。

> minfree2 :: [Int] -> Int
> minfree2 xs = minfrom 0 (length xs, xs)
> 
> minfrom :: Int -> (Int, [Int]) -> Int
> minfrom a (n, xs) | n == 0     = a
>                   | m == b - a = minfrom b (n - m, vs)
>                   | otherwise  = minfrom a (m, us)
>   where
>     (us, vs) = partition (< b) xs
>     b = a + 1 + n `div` 2
>     m = length  us
> 