# 2 データによる抽象の構築
1章では手続きの合成、すなわち合成手続きについて見てきた。本章では、**合成データ(compound data)**について見てゆく。

## 合成データが欲しいのはなぜか？
合成手続きが欲しかったのと同じ理由。プログラム設計の思考レベルを高めるため、設計の部品化力を高めるため、プログラム言語の表現力を広めるため。
合成データが作れると、言語の基本的データより高いレベルの思考でデータを扱える（データの抽象化）。

## 有理数の算術演算を例に
* 有理数の和をつくる演算 add-rat を考える
* add-ratを、和の分子をつくる手続き、分母をつくる手続き、の2つの手続きによるプログラムとして設計することは可能だが、分子と分母の対応を記憶するなど煩雑で、有理数の和を求めたいという当初の目的も見えづらくなる
* **合成データオブジェクト(compound data object)**ー分子と分母を「糊付け」にし、有理数を1個の思考単位と見ることで、プログラムを一貫した方法で操作できるような対

有理数をオブジェクトとして直接扱えると、有理数を整数の対でどう表すかという細部から、プログラムの有理数自身を扱う部分を分離することができる。
このような設計手法を、**データ抽象(data abstraction)**という。データ抽象はプログラムの設計、維持、修正を容易にする。

## 例：「線形結合」ax + by
* 引数a,b,x,yが数値の場合

```Scheme
(define (linear-combination a b x y)
  (+ (* a x) (* b y)))
```

* それ以外（有理数、複素数、多項式etc..）を考慮した場合

```Scheme
(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))
```

addやmulは、引数aやyに渡したデータの種類に見合った演算を行う手続き。
手続きlinear-combinationの視点からは、aやyが何であるかは無関係で、それらが基本データでどう表されているかにも無関係で、知らなければならないのは手続きaddやmulが適切な操作を実行することだけ。
こういった手続きを実装する上で、合成オブジェクトを扱える機能が必要になる。

本章では、有理数算術演算システムの実装を通して、合成データやデータ抽象を考える。
複雑さに対処する技法として、データ抽象による適切な**抽象の壁(abstraction barrier)**というものも出てくる。

その他のキーワード
* **閉包(closure)**
* **公認インターフェース(conventional interface)**
* **記号式(symbolic expressions)**
* **汎用演算(generic operations)**
* **データ主導プログラミング(data-directed programming)**
* **加法的な(additively)**（つまり修正なしの）組み合わせ

本章の終わりでは、多項式上での記号演算を行うパッケージの実装を試みる。

# 2.1 データ抽象入門
データ抽象は、合成データオブジェクトの使い方を、それがより基本的データからどう作られたかの細部から隔離する技法である。
プログラムはデータを, 当面の仕事を実行するのに本当に必要ではないデータについては, 何も仮定しないように使うべきである. 同時に 「具体的な」データ表現は, データを使うプログラムから独立に定義すべきである. システムのこういう二つの部分のインターフェースは**選択子(selectors)**とか**構成子(constructors)**という一組の手続きであって, それらが具体的表現を使い, 抽象データを実装している.

# 2.1.1 例：有理数の算術演算
有理数の足し、引き、掛け、割り算と、等価性のテストを行う。

## **希望的戦略(wishful thinking)**
分子と分母から有理数を作る方法があるとする。また有理数から分子や分母を取り出す(選択する)方法もあるとする。
構成子と選択子は以下のような手続きとして使えるとする。
* (make-rat ⟨n⟩  ⟨d⟩)は分子が整数⟨n⟩, 分母が整数⟨d⟩の有理数を返す.
* (numer ⟨x⟩) 有理数⟨x⟩の分子を返す.
* (denom ⟨x⟩) 有理数⟨x⟩の分母を返す.
これらを用いて足し, 引き, 掛け, 割り, 等価をテストすることが出来る。

```Scheme
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))


(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))


(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
```

numer, denomとmake-rat を定義するには、分子と分母を糊づけにして, 有理数を作る方法が必要。

## 対
データ抽象の具体的レベルが実装出来るよう、基本的手続き consで構成される **対(pair)**という合成構造がある。
consは二つの引数をとり, 二つの引数を部分として含む合成データオブジェクトを返す。対があれば, その部分を基本的手続き carと cdrで取り出すことが出来る。

```Scheme
(define x (cons 1 2))

(car x)
;=> 1

(cdr x)
;=> 2
```

対は基本データオブジェクトのように名前をつけたり操作したり出来る。consは要素が対などであるような対を作るのにも使える。

```Scheme
(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))

(car (car z))
;=> 1

(car (cdr z))
;=> 3
```

対で構成するデータオブジェクトを **リスト構造の(list structured)**データという。

## 有理数の表現
有理数を分子と分母の対で表現するとmake-rat, numerや denomは次のように実装できる。

```Scheme
(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))
```

計算の結果を表示するため, 有理数を分子, 斜線, 分母と 印字する。

```Scheme
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
```

実行結果

```Scheme
(define one-half (make-rat 1 2))

(print-rat one-half)
;=> 1/2

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))
;=> 5/6

(print-rat (mul-rat one-half one-third))
;=> 1/6

(print-rat (add-rat one-third one-third))
;=> 6/9
```

最後の例で有理数を既約にまでは簡約できていないので、make-ratを改善する。
1.2.5節のようなgcd手続きがあって、二つの整数の最大公約数が得られるなら、対を構成する前に gcdを使い、分子と分母を既約にまで簡約出来る。

```Scheme
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third))
;=> 2/3
```

この修正は構成子make-ratの変更だけで実現出来、( add-ratやmul-ratのような)実際の演算を実装している手続きはいずれも変更していない。

問題 2.1
> 正負両方の引数を扱う改良版make-ratを定義せよ. make-ratは符号を正規化し, 有理数が正なら, 分子, 分母とも正, 有理数が負なら, 分子だけ負とする.

```Scheme
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (< d 0)
      (cons (/ (- n) g) (/ (- d) g))
      (cons (/ n g) (/ d g)))))

(make-rat 12 -60)
;=> (-1 . 5)
(make-rat -12 60)
;=> (-1 . 5)
(make-rat -12 -60)
;=> (1 . 5)
(make-rat 12 60)
;=> (1 . 5)
```