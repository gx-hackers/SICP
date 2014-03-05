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

# 2.1.2 抽象の壁
データ抽象の使用側とデータ抽象の実装側を分けるのが**抽象の壁(abstraction barrier)**である。
有理数の例の場合だと、

【壁の例１】
使用側：add-rat, sub-rat, mul-rat, div-rat, equal-rat? を使って有理数を操作する
-------抽象の壁------
実装側：add-rat etc..は、構成子make-rat、選択子number,denom を使って実装される

【壁の例２】
使用側：make-rat etc.. を使ってadd-ratを実装する
-------抽象の壁------
実装側：make-rat etc.. はcons, car, cdr を使って実装される

使用側での操作が不変ならば、実装側の細部はどうなろうと有理数パッケージの他の部分とは無関係である。
consを使おうが使うまいが、壁があるかぎりmake-ratの動作に影響はない。

この壁を適切に設定することで、プログラムのメンテ、改修が容易になる。

例：有理数の約分タイミングを、構成子ではなく選択子に変更する

```Scheme
(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr y) g)))
```

分子や分母を取り出す機会が多いのならmake-ratで約分するのが良いが、そうでないのならば取り出す時まで約分を待ったほうが効率が良くなる。
いずれにしても、make-rat etc.. を使用するadd-rat etc.. を修正する必要はない。
なので、有理数計算の使用状況が分かってからmake-ratの実装方法を決定したり変更することが楽に出来得る。

問題2.2
> 平面上の線分を表現せよ。構成子make-segmentと、選択肢start-segment, end-segmentを定義するべし。更に座標(x,y)を定義する構成子make-pointと、選択子x-point, y-pointを規定せよ。最後に、選択子と構成子を使い、引数として線分をとり中間点を返す手続きmidpoint-segmentを定義せよ。

```Clojure
; 座標の定義
(defn make-point [x y]
  (list x y))
(defn x-point [p]
  (first p))
(defn y-point [p]
  (first (rest p)))

; 線分の定義
(defn make-segment [start-point end-point]
  (list start-point end-point))
(defn start-segment [s]
  (first s))
(defn end-segment [s]
  (first (rest s)))

; 中間点の定義
(defn average [a b]
  (/ (+ a b) 2))
(defn midpoint-segment [s]
  (let [start (start-segment s)
        end (end-segment s)]
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

; 座標の印字
(defn print-point [p]
  (print-str "(" (x-point p) "," (y-point p) ")"))

; 実行
(def start (make-point 1 3))
(def end (make-point 3 5))
(def seg (make-segment start end))
(print-point (midpoint-segment Seg))
;=> "( 2 , 3 )"
```

問題2.3
> 更に長方形を表現せよ。周囲の長さと面積も求めよ。さらにさらに長方形の別の表現も実装せよし、周長と面積が変更なく求められることを確認せよ。

# 2.1.3 データとは何か
データとはなにか？
一般に、データは選択子と構成子と、これらの手続きを有効な表現とするために満たすべき条件とで定義される。

例：cons, car, cdrの場合
任意のオブジェクトx,yについて、zが(cons x y)なら(car z)はxであり、(cdr z)はyである。
という条件を満たせば、"対"というデータオブジェクトを定義できる。
実際、上記３つの手続きは言語の基本機能として実装されているが、自前で実装しても問題ないし、それらは区別できない（上記の条件さえ満たしていれば）。

```Clojure
(defn cons [x y]
  (defn dispatch [m]
    (cond (= m 0) x
          (= m 1) y
          :else (println-str "Argument not 0 or 1 -- CONS" m))))
(defn car [z]
  (z 0))
(defn cdr [z]
  (z 1))

=> (car (cons 23 45))
23
=> (cdr (cons 23 45))
45
```

この場合のconsは手続きをデータとして返す。
この例はまた、手続きをオブジェクトとして操作する能力が、自動的に合成データを表現する能力を提供することを示している。
このデータの手続表現は、**メッセージパッシング(message passing)**といわれ、３章でモデリングとシュミレーションを題材とする時に基本の道具として使用することになる。