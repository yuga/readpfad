[読書メモ][PFAD]

Pearls of Functional Algorithm Design

2 A surpassing problem
======================================

Introduction
--------------------------

以下の記事にあるRemの問題を解く。

Martin Rem (1988a). Small programming exercise 20.
Science of Computer Programming 10 (1). 99-105
http://www.sciencedirect.com/science/journal/01676423/10/1

Martin Rem (1988b). Small programming exercise 21.
Science of Computer Programming 10 (3). 319-325
http://www.sciencedirect.com/science/journal/01676423/10/3

Remによる解法はBinary searchを使ったものだったが、
我々の解法ははdivde and conquerの応用である。

* 定義: (surpasser)
  - A surpasser of an element of an array is a greater element to the right,
    so x[j] is a surpasser of x[i] if i < j and x[i] < x[j].

* 定義: (surpasser count)
  - The surpasser count of an element is the number of its surpassers.

<h4>例:</h4>
    G  E  N  E  R  A  T  I  N  G
    5  6  2  5  1  4  0  1  0  0

この例におけるsurpasser countの最大値は6。
ひとつめのEがN R T I N Gの6個のsurpasserを持つ。

Remの問題は、
    to compute the maximum surpasser count of an array of length n > 1
    and to do so with an O(n log n) algorithm.

<h3>Specification</h3>

配列で考えるのはやめてリストということにする。

> import Data.List
> 
> msc1 :: Ord a => [a] -> Int
> msc1 [] = 0
> msc1 xs = maximum [scount1 z zs | z:zs <- tails1 xs]
> 
> scount1 :: Ord a => a -> [a] -> Int
> scount1 x xs = length (filter (x <) xs)
> 
> tails1 :: [a] -> [[a]]
> tails1 []         = []
> tails1 xs@(_:xs') = xs : tails1 xs'

mscは定義可能だが指数時間かかってしまう。

    - tails1  : O(n)
      scount1 : O(n)
      => msc1 : O(n^2)


Divide and conquer
--------------------------

If we can find a function join so that

    msc (xs ++ ys) = join (msc xs) (msc ys)

and join can be computed in linear time, then the time complexity T(n) of
the divde and conquer algorithm for computing msc on a list of length n
satisfies T(n) = 2T(n/2)+O(n), with solution T(n) = O(n log n).

mscはカウント結果しか提供しないので情報が少なすぎて分解が行えない。

    アルゴリズムイントロダクション第3章で述べられているmaster theoremを用いて、
    漸化式を以下のように計算できる。

        a >= 1 と b > 1 を定数、f(n)を関数とする。非負整数上の関数T(n)を漸化式
            T(n) = aT(n/b) + f(n)
        によって定義する。ここでn/bはfloor(n/b)またはceil(n/b)を意味するものとする。
        このとき、T(n)は漸近的につぎのような限界を持つ。

        1. ある定数ε>0に対してf(n) = O(n^(log_b(a-ε)))ならばT(n) = Θ(n^(log_b(a)))である。
        2. f(n) = Θ(n^(log_b(a)))ならば、T(n) = Θ(n^(log_b(a)) lg n)である。
        3. ある定数ε > 0に対してf(n) = Ω(n^(log_b(a+ε)))であり、
           しかもある定数c < 1と十分大きなすべてのnに対してaf(n/b) <= cf(n)ならば、
           T(n) = Θ(f(n))。

    master methodの2.から、分割した個々の部分をO(n)時間で計算できればO(n log n)になることがわかる。
    master methodを使わなくても、ツリーを描いて計算してもいい。

まず最低限の一般化として、すべてのsurpasser countを計算したtableを作るところから始める。
(分解可能になるよう情報をすべてもったデータを作る)

> msc1' :: Ord a => [a] -> Int
> msc1' = maximum . map snd . table1'
> table1' xs = [(z, scount1 z zs) | z:zs <- tails1 xs]

以下の等式を満たす線形時間joinを作ることが出来る。

    table1' (xs ++ ys) = join (table1' xs) (table1' ys)

このためにはtails関数の以下のdivde and conquerの性質が必要である。

    tails1 (xs ++ ys) = map (++ys) (tails xs) ++ tails ys

table1''の式から以下のように導出できる。

      tables (xs ++ ys) 
    =   { definition }
      [(z,scount z zs) | z:zs <- tails (xs ++ ys)]
    =   { divide and cnquer property of tails }
      [(z,scount z zs) | z:zs <- map (++ ys) (tails xs) ++ tails ys]
    =   { distributing <- over ++ }
      [(z,scount z (zs ++ ys)) | z:zs <- tails xs] ++ [(z,scount z zs) | z:zs <- tails ys]
    =   { since scount z (zs ++ ys) = scount z zs + scount z ys }
      [(z,scount z zs + scount z ys) | z:zs <- tails xs] ++ [(z,scount z zs) | z:zs <- tails ys]
    =   { definition of table and ys = map fst (table ys) }
      [(z,c + scount z (map fst (table ys))) | (z,c) <- table xs] ++ table ys

結果をまとめると以下のようになる。

> msc2 :: Ord a => [a] -> Int
> msc2 [] = 0
> msc2 xs = maximum $ map snd $ table2 xs
> 
> table2 :: Ord a => [a] -> [(a,Int)]
> table2 [x] = [(x,0)]
> table2 xs  = join2 (table2 ys) (table2 zs)
>   where
>     m       = length xs
>     n       = m `div` 2
>     (ys,zs) = splitAt n xs
> 
> join2 :: Ord a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
> join2 txs []  = txs
> join2 []  tys = tys
> join2 txs tys = [(z, c + tcount2 z tys) | (z,c) <- txs] ++ tys
> 
> tcount2 :: Ord a => a -> [(a,Int)] -> Int
> tcount2 z tys = scount1 z (map fst tys)

このコストは以下の通り。

    - tcount2     : O(n)
      => join2    : O(n^2)
        => table2 : O(n^2) 

join2がtxs、tysに対してlinear timeでない。
tcount2の2番目の引数tysがtupleの1番目のコンポーネントに対して小さい方から順の
ソート済みリストであれば、関数tcount2のコストを以下のようにして削減できる。

    tcount2' z tys = length (dropWhile ((z >=) . fst) tys)  -- (2.1)

この関数が要する実際のコストについて解析は置いておくとして、
joinにソート済みリストを渡すため、tableでソートを行う。

    table1'' xs = sortBy (\a b -> compare (fst a) (fst b)) [(z,scount z zs) | z:zs <- tails1 xs]

table1'をもとにすると上記のようになる。
ソート済みリストを扱うため、join2は以下のような変更をすることになる。

    join2' txs tys = [(x,c + tcount x tys) | (x,c) <- txs] `merge` tys
      where
        merge = (++)

ここは、全体を以下のように書くとわかりやすい。
table関数はtable2をもとに書き直した。

> msc2' :: Ord a => [a] -> Int
> msc2' [] = 0
> msc2' xs = maximum $ map snd $ table2' xs
> 
> table2' :: Ord a => [a] -> [(a,Int)]
> table2' [x] = [(x,0)]
> table2' xs  = sort' (join2' (table2' ys) (table2' zs))
>   where
>     m       = length xs
>     n       = m `div` 2
>     (ys,zs) = splitAt n xs
>     sort'   = sortBy (\a b -> compare (fst a) (fst b))
> 
> join2' :: Ord a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
> join2' txs []  = txs
> join2' []  tys = tys
> join2' txs tys = [(x,c + tcount2' x tys) | (x,c) <- txs] `merge` tys
>   where
>     merge = (++)
> 
> tcount2' :: Ord a => a -> [(a,Int)] -> Int
> tcount2' z tys = length (dropWhile ((z >=) . fst) tys)

table2'は受け取った未ソートのリストを2分割して、それぞれを自己再帰でソートしてから
join2'で連結し、結果をソートして返す関数で、
join2'はソート済みリストを2つ受け取り、surpasser countを計算しながら、
両方をマージして、1つのソート済みリストを返す関数である。

2つの関数の機能をあわせると、これは merge sort + surpasser count といえる。
merge関数の実装は上記実装では単にリストを連結する (++) であるが、
リストを要素を適切な順でマージすることでtableでのソートが不要になる。

> join2'' txs tys = [(x,c + tcount2' x tys) | (x,c) <- txs] `merge` tys
>   where
>     merge tas []  = tas
>     merge []  tbs = tbs
>     merge tas@((a,sa):tas') tbs@((b,sb):tbs')
>       | a < b     = (a,sa) : merge tas' tbs
>       | otherwise = (b,sb) : merge tas  tbs'

さらにjoin2'の式を再帰を使ったより効率的な実装に書き換えてmerge関数をなくすことができる。
このために、まず join の左辺を以下のように変更して、両方の先頭要素を取得できるようにする。

    join2''' txs@((x,c):txs') tys@((y,d):tys') = undefined   -- (2.3)

マージとsurpasser countを計算を同時に行うために、xとyの比較結果別に
必要な計算を検討する。

* x < y の場合
  - txsの先頭要素を新しいリストの先頭要素とする。
    (2.1) より tcount2' x tys = length tys (ここのxとtysはjoin2'''のもの) だから (2.3) は
        (x,c + length tys) : join''' txs' tys
    となる。

* x = y の場合
  - join2' の c + tcount2' x tys と join2''' の d を比較する必要がある。
    もともとのtableの定義(table1')から {join2''' の} d = {join2 の} tcount2 x tys であり、
    (2.1)から tcount2' x tys = tcount2' x tys' である(ここのxと tys, tys'はjoin2'''のもの)。
    そこでさきにtxsの先頭要素を新しいリストに入れてから、次にtysの先頭要素を入れることにすると、
        (x,c + tcount2' x tys) : (y,d) : join2''' txs' tys'
    となり、これは
        (x,c + tcount2' x tys) : join2''' txs' tys
    と等しい。
    反対にさきにtysの先頭要素を新しいリストに入れてから、次にtxsの先頭要素を入れることにすると、
        (y,d) : (x,c + tcount2' x tys') : join2''' txs' tys'
    となり、これは
        (y,d) : join2''' txs tys'
    と等しい。後者の方がシンプルなのでこちらを採用する。

* x > y の場合
  - tys側の先頭要素を新しいリストに入れるべきなので、x = y の場合で採用したものと同じになる。

以上により、join2'''は以下のようになる。

> join2''' txs []  = txs
> join2''' []  tys = tys
> join2''' txs@((x,c):txs') tys@((y,d):tys')
>   | x <  y = (x,c + length tys) : join2''' txs' tys
>   | x >= y = (y,d) : join2''' txs tys'

リストtysの長さを何度も計算するのを避けるために、tysの長さをjoin関数の引数にする。
その結果、最終形は以下のようになる。

> msc3 :: Ord a => [a] -> Int
> msc3 [] = 0
> msc3 xs = maximum $ map snd $ table3 xs
> 
> table3 :: Ord a => [a] -> [(a,Int)]
> table3 [x] = [(x,0)]
> table3 xs  = join3 (m - n) (table3 ys) (table3 zs)
>   where
>     m       = length xs
>     n       = m `div` 2
>     (ys,zs) = splitAt n xs
> 
> join3 :: Ord a => Int -> [(a,Int)] -> [(a,Int)] -> [(a,Int)]
> join3 0 txs []  = txs
> join3 _ []  tys = tys
> join3 n txs@((x,c):txs') tys@((y,d):tys')
>   | x <  y = (x,c+n) : join3 n txs' tys
>   | x >= y = (y,d) : join3 (n-1) txs tys'

join3はlinear timeしかかからないので、tableはO(n log n)で計算される。
mscもまた同じである。

