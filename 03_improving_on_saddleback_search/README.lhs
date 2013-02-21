[読書メモ][PFAD]

Pearls of Functional Algorithm Design
 
3 Improving on saddleback search 
======================================

問題の定義
---------------------------------------
2つの引数をとる関数invertを設計する。

> invert :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> invert f z = undefined

* f : 自然数のペアから自然数を返す関数。返す結果は各引数よりも
      確実に増加(is strictly increasing in each argument)し、
      それ以外はない。f (x,y) = z のとき、x <= z かつ y <= z。
* z : 自然数。
* invert f z : f (x,y) = z を満たす、すべての(x,y)のペアのリスト。


解法(1) 定義から導かれる関数invertの素直な実装
---------------------------------------
> invert1 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> invert1 f z = [(x,y) | x <- [0..z], y <- [0..z], f (x,y) == z]

**コスト:**

invert1はfを(z+1)^2回評価する可能性がある。


解法(2) 少しの改造でコストを半減させるた実装
---------------------------------------
fが増加関数ならばf (x,y) >= x + y となる。
よって平方の対角線以下に値は制限される。

**f (x,y) >= x + y の証明:**<br />
任意の自然数aについて x = a であるとき、yに対しての帰納法を用いる。
y = 0 のとき z >= x より z >= a であるから、f (a,0) >= a + 0 となり
証明の命題は正しい。

y >= k のとき f (x,k) >= x + k が成り立つと仮定し、y = k + 1のとき、
f (x,k+1) >= x + (k+1)であることを証明する。(x+(k+1)) - (x+k) = 1で
あるので、f (x,k+1) - f (x,k) >= 1を証明すればよい。まず、fは増加関数
であるからf (x,k+1) > f (x,k)となる。このときfは自然数を返すので
f (x,k+1) - f (x,k) >= 1である。

> invert2 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> invert2 f z = [(x,y) | x <- [0..z], y <- [0..z-x], f (x,y) == z]

**コスト:**
この解決方法は実行コストのオーダーに影響しない。


解法(3) 探索範囲を狭める方法を採用した実装
----------------------------------------
これまでの方法はサイズがz+1の正方形を左下の原点(0,0)から1カラムごと
値を増やしながら探索している。

正方形の左上の角(0,z)から開始しするとより良くできる。
各段階で探索範囲を左上の角(u,v)と右下の角(z,0)の矩形に制限する。

<!--
(0,z)    (z,z)
┌────┐
│ (u,v)  │
│  ┌──┤
│  │ ※ │ ※探索範囲
│  │    │
└─┴──┘
(0,0)    (z,0)
-->

> invert3 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> invert3 f z = find3 (0,z) f z
>
> find3 :: (Int,Int) -> ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> find3 (u,v) f z = [(x,y) | x <- [u..z], y <- [v,v-1..0], f (x,y) == z]

コスト: 未解析


解法(4) Saddleback Search (基本形)
-----------------------------------------
* まずはじめに、u < z または v < 0 のとき明らかに find (u,v) f z = [] である。
* つぎに f (u,v) < z ならば、すべての v' < v に対し f (u,v') < f (u,v) < z だから、
  この列uの残りを解から除去する。
* f (u,v) > z ならば、同様にこの行vの残りを解から除去する。
* 最後に f (u,v) = z ならば、(u,v) を記録して、列uと行vの残りを解から除去する。

<!--
(0,z)    (z,z)
┌────┐
│ (u,v)  │
│  ┌──┤
│  │    │
│  │    │
└─┴──┘
(0,0)    (z,0)
-->

> invert4 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> invert4 f z = find4 (0,z) f z
> 
> find4 :: (Int,Int) -> ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> find4 (u,v) f z 
>     | u > z || v < 0 = []
>     | z' <  z        = find4 (u+1,v) f z
>     | z' == z        = (u,v) : find4 (u+1,v-1) f z
>     | z' >  z        = find4 (u,v-1) f z
>   where
>     z' = f (u,v)

コスト: 最悪の場合、findは左上の角から右下の角まで正方形の周囲を巡回することになるので、
fの評価回数は 2z + 1 である。最良の場合、findは底または右端に直接進むので、z + 1 の
評価回数が必要である。


解法(5) Saddleback Search (探索範囲を削減)
------------------------------------------
まだ探索範囲を削減できる。初期の探索範囲を正方形の左上の角(0,z)と
右下の角(z,0)にしているが、求める値に対して過剰である。

(z+1)*(z+1)の正方形を探索するよりも<br/>
(m+1)*(n+1)の矩形を探索するようにする。
  
> invert5 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> invert5 f z = find5 (0,m) f z
>   where
>     m = maximum (filter (\y -> f (0,y) <= z) [0..z])
> 
> find5 :: (Int,Int) -> ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> find5 (u,v) f z
>     | u > n || v < 0 = []
>     | z' <  z        = find5 (u+1,v) f z
>     | z' == z        = (u,v) : find5 (u+1,v-1) f z
>     | z' >  z        = find5 (u,v-1) f z
>   where
>     n  = maximum (filter (\x -> f (x,0) <= z) [0..z])
>     z' = f (u,v)

コスト: 未解析


解法(6) Saddleback Search (探索範囲の決定に二分探索を利用)
------------------------------------------
さらにmとnを二分探索で求めることができる。
自然数上の増加関数gに対し、g x <= z < g y となるx、y、zを仮定する。
一意な値mを決定する m = bsearch g (x,y) z は
g m <= z < g (m+1)であるようなx <= m < yの範囲で
g a <= z < g b かつ x <= a < b <= y の不変式を維持できるものとなる。

整理すると g x <= g a <= g m <= z < g (m+1) <= g b <= g y である。
これは以下のプログラムを導く。

> bsearch g (a,b) z
>    | a+1 == b  = a
>    | g m <= z  = bsearch g (m,b) z
>    | otherwise = bsearch g (a,m) z
>   where
>     m = (a+b) `div` 2

a + 1 < b => a < m < y なので、bsearch g (x,y) z を評価したとき、
g x と g y はどちらも、このbserchのアルゴリズムのなかで実際に
評価されることはない。

したがって(m+1)*(n+1)の矩形のmとnを求めるとき、基点となる(x,y)を
以下のように設定する。

    m = bsearch (\y -> f (0,y)) (-1,z+1) z
    n = bsearch (\x -> f (x,0)) (-1,z+1) z

ここでのfはinvertの引数であるfだが、bsearchに使用するときは
仮想の値 f (0,-1) = 0 かつ f (-1,0) = 0 を持つよう拡張する。

> invert6 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> invert6 f z = find6 (0,m) f' z
>   where
>     m = bsearch (\y -> f' (0,y)) (-1,z+1) z
>     f' (0,-1) = -1
>     f' (-1,0) = -1
>     f' = f
> 
> find6 :: (Int,Int) -> ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> find6 (u,v) f z
>     | u > n || v < 0 = []
>     | z' <  z        = find6 (u+1,v) f z
>     | z' == z        = (u,v) : find6 (u+1,v-1) f z
>     | z' >  z        = find6 (u,v-1) f z
>   where
>     n  = bsearch (\x -> f (x,0)) (-1,z+1) z
>     z' = f (u,v)

コスト: このバージョンのintertは、fの最悪評価回数 2log(z)+m+n、
最良評価回数 2log(z)+min(m,n)である。mまたはnは大体において
zより小さくなりうるので、(たとえば f (x,y) = 2^x + 3^y )
結果としてアルゴリズムは最悪の場合でもO(log(z))しかかからない。


解法(7) Divide and Conquer
------------------------------------------
__戦略__

左上の角(u,v)と右下の角(r,s)に探索する矩形を制限するとする。
f (u,v)に着目するより p = (u+r) div 2 かつ q = (u+s) div 2 であるところの
f (p,q) を調べてみることにする。

<!--
(u,v)           (r,v)
┌───┬───┐
│ (p,q)│  B   │
├───┼───┤
│  A   │      │
└───┴───┘
(u,s)           (r,s)
-->

f (p,q) < z であるなら、左下の矩形Aの要素はすべて放棄する。
同様に f (p,q) > zであるなら、右上の矩形Bの要素はすべて放棄する。
f (p,q) = z であるなら、両方放棄する。

<!--
                  n                    ceil(n/2)  floor(n/2)
          ┌───────┐             ┌───┬───┐ ┌───┬───┐
 ceil(m/2)│    ノコス    │   floor(m/2)│ノコス│ステル│ │ノコス│ステル│
          ├───┬───┤             ├───┴───┤ ├───┼───┤
floor(m/2)│ステル│ノコス│    ceil(m/2)│    ノコス    │ │ステル│ノコス│
          └───┴───┘             └───────┘ └───┴───┘
      floor(n/2)   ceil(n/2)                     n
-->

この戦略は、探索範囲が矩形である、という不変式を維持しない。
2つの矩形またはL字型の領域が残る。しかしこれらは再帰的に計算可能である。

L字型は水平または垂直に2つの矩形に分割できる。

コスト:

__上限:漸化式Tを定義する__

m*nの矩形があるとき、T(m,n)はその矩形を探索するのに必要なfの評価回数を
表すとする。

<pre>
m = 0 または n = 0 であるなら、探索するものはない。

m = 1 または n = 1 であるなら、
  T(1,n) = 1 + T(1,ceil(n/2))
  T(m,1) = 1 + T(ceil(m/2),1)

m >= 2 かつ n >= 2 のとき、
少なくとも floor (m/2) * floor (n/2)のサイズの領域1つを放棄する。
水平に分割を行うと、floor(m/2) * ceil(n/2) と ceil(m/2) * n のサイズの
2つの矩形が残る。
</pre>

したがって、

    T(m,n) = 1 + T(floor(m/2),ceil(n/2)) + T(ceil(m/2),n)

垂直に分割を行うと

    T(m,n) = 1 + T(ceil(m/2),floor(n/2)) + T(m,ceil(n/2))


__上限:漸化式Tの解を求める__

水平、垂直のいずれの分割も、3つの矩形が残るので、m >= 2 かつ n >= 2 のとき

    T(m,n) = 1 + T(ceil(m/2),floor(n/2))
               + T(ceil(m/2),ceil(n/2))
               + T(floor(m/2),ceil(n/2))

この再帰を解くことができる。U(i,j) = T(2^i,2^j)とすると

    U(i,0)     = i             <<< T(2^i,1)なので n = 1 のケース
    U(0,j)     = j             <<< T(1,2^j)なので m = 1 のケース
    U(i+1,j+1) = 1 + 3U(i,j)   <<< m >= 2 かつ n >= 2 のケース

帰納法で確認できる解として

    U(i,j) = (3^k) * (|j-i| + 1/2) - 1/2 where k = i min j

    上記に至る展開:
      定義から以下の導出を行う。
        U(i+1,j+1) = 1 + 3U(i,j)
         => U(i,j) = 1 + 3U(i-1,j-1)
                   = 1 + 3(1 + 3U(i-2,j-2))
                   = 1 + 3(1 + 3(1 + 3U(i-3,j-3)))
                   = 1 + 3(1 + 3(1 + 3(1 + ... 3U(0,j-i) ...)))      <<< if i <= j
                   = 1 + 3 + 9 + 27 + .. + 3^(i-1) + (3^i)*U(0,j-i)
                   = sum_x=0^(i-1){3^x} + (3^i)*U(0,j-i)
                   = (3^i - 1)/2 + (3^i)*U(0,j-i)
                   = (3^i)/2 - 1/2 + (3^i)*U(0,j-i)
                   = (3^i)/2 - 1/2 + (3^i)*(|j-i|)                   <<< U(0,j-i) ならば |j-i|
                   = (3^i)*(|j-i| + 1/2) - 1/2

したがって m <= n ならば

    T(m,n) <= (3^log(m))*log(2n/m) = (m^1.59)*log(2n/m)
    上記に至る展開:
      T(m,n) = U(log(m),log(n))
             = (3^log(m)) * (log(n) - log(m) + 1/2)
             = (3^log(m)) * (log(n/m) + log(sqrt(2)))
             = (3^log(m)) * log(sqrt(2)*n/m)
             <= (3^log(m)) * log(2n/m)                  <<< sqrt(2) <= 2
             <= (m^1.59) * log(2n/m)                    <<< 3^log(m) = m^log(3)

これはmがnより一層小さいときほど m + n より良くなる。


__下限__

m*n矩形のときに可能な出力を考える。

A(m,n)個の異なる解があると仮定すると、zに対するf (x,y)のテストは3つの可能性があり、
テストの3分木の高さはhは h >= log3(A(m,n)) を満たさなければならない。
A(m,n)が評価可能であれば、実行しなければならないテストの下限を求めることができる。
その状況は、は2項比較によるn個の要素のソートと同じである。
n!個の可能な結果があり、どんなソートアルゴリズムも最悪の場合少なくとも
log2(n!)=Θ(n*log(n))回の比較が必要である。

A(m,n)を評価するのは簡単で、0 <= x < n と 0 <= y < m の範囲で f (x,y) = z な
(x,y)のペアの各リストが、m*n矩形の左上の角から右下の角へ1段階ごとの形で
1対1対応する。そこでは値zが各段階の内側の角に現れる。もちろん、このステップごとの
形は関数findでトレースする必要はない。そのような経路の数は((m+n) choose n)あり、
それがA(m,n)の値である。

また別の方法では、k個の解があると仮定する。
値zは、ちょうど(m choose k)通りなかでk個に現れることができて、
各1通りごとに(n choose k)個の選択が列に対して可能である。
したがって

    A(m,n) = sum_k=0^m{(m choose k)*(n choose k)} = ((m+n) choose n)

* wolframalpha:
  * sum_k=0^m((m!/(k!*(m-k)!))*(n!/(k!*(n-k)!)))

* the summation is an instance of Vandermonde's convolution;
  * http://en.wikipedia.org/wiki/Vandermonde's_identity
  * Graham, R. L., Knuth, D. E. and Patashnik, O. (1989).
    Concrete Mathematics. Reading, MA: Addison-Wesley.

対数をとると、下限が得られる

  log A(m,n) = Ω(m*log(1+n/m) + n*log(1+m/n))

この評価はm=nのときΩ(m+n)より良くなることができない。
しかし 0 <= x <= 1のとき x <= log(1+x) なので、m <= n ならば m <= n*log(1 + m/n) である。
したがって log A(m,n) = Ω(m*log(n/m))となる。


__最終的な実装の検討__

この問題を解くのに二分探索を使用する方法が他にも2つある。
ひとつはm回の二分探索を単に実行するだけで、ひとつは各行で行う。
どちらも O(m*log(n)) となるが、m <= nを仮定すると、
最良でO(m*log(n/m))の漸近的上限を達成できる。

これまでの方法と同様に、探索範囲を左上の角(u,v)と右下の角(r,s)の矩形に限定する。
したがって、r-u列とv-s行ある。さらに v-s <= r-u (列より行が少ない)を仮定すると、
少なくとも行と同じだけの列があることになる。

f (p,q) <= z < f (p+1,q) のような p をひとつ決定するために
中央の行 q = (v+s) div 2 にそって二分探索を実行したとする。

<!--
      (u,v) 
       ┌─────┬─────┐
       │          │          │
       │(u,q)     │(p,q)     │
       ├─────┼─────┤
       │          │          │
       │          │          │
       └─────┴─────┘
                             (r,s)
-->

    f (p,q) < z の場合:
      ((u,v),(p,q+1)) と ((p+1,q-1),(r,s)) の2つ矩形上だけで
      探索を続行する必要がある。

    f (p,q) = z の場合:
      列pを削除し、((u,v),(p-1,q+1)) と ((p+1,q-1),(r,s)) の矩形上だけで
      探索を続行する必要がある。

    (列より行の方が多い場合、その試行回数は2倍)

結果として対数回の探索で配列の約半分の要素を除外できる。

実装:

> invert7 :: ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> invert7 f z = find7 (0,m) (n,0) f' z
>   where
>     m = bsearch (\y -> f' (0,y)) (-1,z+1) z
>     n = bsearch (\x -> f' (x,0)) (-1,z+1) z
>     f' (0,-1) = 0
>     f' (-1,0) = 0
>     f' = f
> 
> find7 :: (Int,Int) -> (Int,Int) -> ((Int,Int) -> Int) -> Int -> [(Int,Int)]
> find7 (u,v) (r,s) f z
>     | u > r || v < s = []
>     | v - s <= r - u = rfind (bsearch (\x -> f (x,q)) (u-1,r+1) z)
>     | otherwise      = cfind (bsearch (\x -> f (p,y)) (s-1,v+1) z)
>   where
>     p       = (u+r) `div` 2
>     q       = (v+s) `div` 2
>     rfind p = (if f (p,q) == z then
>                    (p,q) : find7 (u,v) (p-1,q+1) f z
>                else
>                    find7 (u,v) (p,q+1) f z)
>               ++
>               find7 (p+1,q-1) (r,s) f z
>     cfind q = find7 (u,v) (p-1,q+1) f z
>               ++
>               (if f (p,q) == z then
>                    (p,q) : find7 (p+1,q-1) (r,s) f z
>                else
>                    find7 (p+1,q) (r,s) f z)

find (u,v) (r,s) f z は左上の角(u,v)と右下の角(r,s)の矩形を探索する。

コスト: m*n矩形を探索するのに必要な評価回数をT(m,n)とし、m <= n を仮定すると、
最良の場合、たとえば行の上での各二分探索が左端または右端の要素を返すならば、
T(m,n) = log(n) + T(m/2,n) で、T(m,n) = O(log(m)*log(n)) となる。
最悪の場合、各二分探索が中央の要素を返し、

    T(m,n) = log(n) + 2T(m/2,n/2)

これを解くには、U(i,j) =T(2^i,2^j) とおいて

    U(i,j) = sum_k=0^i-1((2^k)*(j-k)) = O((2^i)*(j-i))

したがって T(m,n) = O(m*log(n/m))
これは先ほど求めた漸近的な下限とも一致する。

