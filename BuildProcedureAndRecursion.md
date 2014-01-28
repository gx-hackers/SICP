# 1.1.7 例：Newton法による平方根
## 平方根の計算
* 平方根の関数の数学的定義
 * ルートx == (y >= 0)　かつ　y*y = x であるようなy
 * 上記は、完全に正当な数学の関数を述べている
 * Lisp風に書いてみる

```Scheme
(define (sqrt *)
  (the y (and (>= y 0)
              (= (square y) x))))
```

 * この式は、与えられた数の平方根をどう見つけたらいいかについてほとんど何も言っていない（手続きを述べていない）
  * 関数（数学）：もののあり様の記述、平叙文的知識
  * 手続き（プログラミング）：ことのなし方の記述、命令文的記述

### （手続き的な）平方根の計算
* Newton法（の一種）を使用した計算方法
 * 数xの平方根の値の予測値yがあれば、xとx/yの平均値を取るという計算で、真の平方根により近い予測値が得られる
 * x=2、y=1 での計算（本文P.13）
* Lisp(Clojure)による手続きの実装

```Clojure
(defn average [x y]
  (/ (+ x y) 2))

(defn improve
  "予測値は、それ自身と、被開平数とそれ自身の商の、平均として改善される"
  [guess x]
  (average guess (/ x guess)))

(defn good-enough?
  "予測値の2乗と被開平数の差が十分小さいかテストする"
  [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))　; 自分自身を再度呼び出す＝再帰ループ

(defn sqrt
  "予測値1.0から開始する"
  [x]
  (sqrt-iter 1.0 x))
```

* 計算結果

```
user=> (sqrt 9)
3.00009155413138

user=> (sqrt (+ 100 37))
11.704699917758145

user=> (sqrt (+ (sqrt 2) (sqrt 3)))
1.7739279023207892

user=> (square (sqrt 1000))
1000.000369924366
```

* "for"などのループ構文がなくても、再帰を利用して反復計算を実行できる

### 参考（Clojure）
* 関数型っぽく書くなら、再帰よりも遅延シーケンスを使う

```Clojure
(defn sqrt2 [x]
  (defn good-enough? [guess]
    (< (Math/abs (- (square guess) x)) 0.001))

  (defn improve [guess]
    (average guess (/ x guess)))

  (defn sqrt-seq
    "被開閉数xの平方根候補の無限遅延リストを返す"
    [guess]
    (iterate (partial improve) guess))

  (defn sqrt-filt
    "遅延リストにgood-enough?フィルターを適応する"
    [guess]
    (first (take 1 (drop-while (complement (partial good-enough?)) (sqrt-seq guess)))))

  (sqrt-filt 1.0))
```

* 計算結果

```
user=> (sqrt2 2)
1.4142156862745097

user=> (sqrt2 9)
3.00009155413138
```

# 問題コーナー
## 問題1.6
* "if"が特殊形式でなければならない理由は？
* (問題1.5とおんなじですね。)

## 問題1.7
* good-enough?テストは、非常に小さな数には有効ではない

```
user=> (sqrt 0.0004)
0.0354008825558513

user=> (square (sqrt 0.0004))
0.0012532224857331766
```

* 同様に、非常に大きな数にも不適切である

```
user=> (sqrt2 10000000000000)
無限ループ
```

* より適切なgood-enough?テストの実装を考える
 * "ある繰り返しから次へのguessの変化に注目し、変化が予測値に比べ非常に小さくなった時に計算を止める"

```Clojure
(defn good-enough-imp?
  "前回の予測値と今回の予測値の変化量に注目する"
  [guess previous-guess]
  (< (Math/abs (- 1 (/ guess previous-guess))) 0.001))

(defn sqrt-iter-imp [guess previous-guess x]
  (if (good-enough-imp? guess previous-guess)
    guess
    (sqrt-iter-imp (improve guess x) guess x)))

(defn sqrt-imp [x]
  (sqrt-iter-imp 1.0 10.0 x))
```

* 計算結果

```
user=> (sqrt-imp 0.0004)
0.020000000050877154
user=> (square (sqrt-imp 0.0004))
4.0000000203508615E-4

user=> (sqrt-imp 10000000000000)
3162277.6640104805
user=> (square (sqrt-imp 10000000000000))
1.0000000024299582E13
```

## 問題1.8
* 立方根を求めるNewton法

```Clojure
(defn improve-crt [guess x]
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3))

(defn crt-itr [guess previous-guess x]
  (if (good-enough-imp? guess previous-guess)
    guess
    (crt-itr (improve-crt guess x) guess x)))

(defn crt [x]
  (crt-itr 1.0 10.0 x ))

(defn cube [x]
  (* x x x))
```

* 計算結果

```
user=> (crt 10)
2.154434691772293
user=> (cube (crt 10))
10.000000024234794

user=> (crt 27)
3.0000005410641766
user=> (cube (crt 27))
27.000014608735402
```

# 1.1.8 ブラックボックス抽象としての手続き
* プログラミング経験者には今更な内容ですが…
* 平方根を計算する手続き"sqrt"は、以下の手続きの束と見ることができる(図1.2)
 * sqrt-iter, good-enough?, square, abs, improve, average
 * これらの束は、問題の部分問題への分割を反映している
* 分割戦略は単にプログラムを部分に分けることより重要である
 * 各手続きは、他の手続きを定義するとき、その部品として使え、まとまった仕事ができる
* squareを使ってgood-enough?を定義するとき、squareは「ブラックボックス」と見ることができる
 * squareを使う時に関心を持つのは、どう結果を計算するかではなく、それが２乗を計算するという事実だけ
 * squareは手続きではなく、手続き抽象(procedural abstraction)とも言える
 * 以下の抽象手続きはどれも、数の引数をとり、その数の２乗を値として作る、という意味で区別できない

```Scheme
(define (square x) (* x x))

(define (square x)
  (exp (double (log x))))

(define (double x) (+ x x))
```

これらのうちどれが効率良い実装かは自明ではなく、使用する計算機に依存する（例：対数と真数の大きな表を効率よく記憶している計算機を考えてみると？）
* 手続き定義は細部を隠すことができる
 * 手続き利用者は、手続き自身を書かずに他のプログラマからブラックボックスとしてもらってくれば良い
 * 利用者はその使用にあたって、手続きの実装の内容を知る必要がない

## 局所名
* 手続き利用者が知らなくていい実装上の細部
 * 手続きの仮パラメタ（仮引数）の名前

```Scheme
(define (square x) (* x x))

(define (square y) (* y y))
```

上記の手続きは区別できない
* 仮パラメタ名は、手続き本体に対して局所的でなければならない

```Scheme
(define (square x) (* x x)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
```

* squareの引数xの値が、good-enough?の第二引数xの値に影響を与えてはならない
 * そうでないならsquareをブラックボックスとして扱うことができない

### パラメタの束縛
* 手続きの定義の中で、仮パラメタはどんな名前を持っても構わない
 * パラメタ名を変更しても、手続き定義の意味は変わらない
* **束縛変数(bound variable)**
* 手続き定義は仮パラメタを**束縛(bind)**する
* 変数が束縛されていない = **自由である(free)**
* 名前が束縛されている式の範囲を**有効範囲(scope)**という
 * 束縛変数のscopeは手続き本体内に限定される
* good-enough?の場合
 * 自由変数：< - abs square
 * 束縛変数：guess x
 * guessやxは、自由変数と異なる限りどんな名前を選んでも、手続きの意味に影響を与えない
 * guessをabsとした場合、自由変数absが**捕捉(capture)**され束縛変数に変わり、バグが生まれる
 * 変数absは、手続き定義外で定義された、数の絶対値を計算する手続きの名前
 * 自由変数の名前は手続き自体の意味に影響する
 * good-enough?内でabsをcosに置き換えると？

## 内部定義とブロック構造

```Scheme
(define (sqrt x)
  (..........))

(define (sqr-itr guess x)
  (..........))

(define (good-enough? guess x)
  (..........))

(define (improve guess x)
  (..........))
```

### 内部定義によるブロック構造化
* sqrtの利用者にとって、重要な手続きはsqrtのみ
 * にもかかわらず、このままではsqr-itrやgood-enough?という名前の別の手続きを定義することができない（再定義は可能だが、上書きされる）
* 局所的な内部定義を可能にすることで、sqrtのgood-enough?が他の手続き内の別のgood-enough?と共存できるようになる

```Scheme
(define (sqrt x)
  (define (good-enough? guess x)
    (..........))
  (define (improve guess x)
    (..........))
  (define (sqr-itr guess x)
    (..........))
  (sqrt-iter 1.0 x))
```

このような定義の入れ子を**ブロック構造(block structure)**といい、最も単純な名前保護の機構である
* 組み込まれた定義は、手続き本体の先頭になければならない（定義はその使用よりも先になければならない）
* ブロック構造はAlogol 60で始まり、現代の言語の殆どに見られ、巨大プログラム構築作業に必須な役割を担っている

### 変数の静的有効範囲
* xはsqrtの定義に束縛されている
* sqrt内部定義のgood-enough?、improve、sqr-iter手続きは、xの有効範囲内にある
 * すなわちxをこれら手続きに明示的に渡す必要はない

```Scheme
(define (sqrt x)
  (define (good-enough? guess)
    (....x......))
  (define (improve guess)
    (......x....))
  (define (sqr-itr guess)
    (..........))
  (sqrt-iter 1.0 x))
```

上記のように、xを内部定義で自由変数にすると、xは外側の手続きsqrtの仮パラメタに束縛された値をとる。このやり方を**静的有効範囲(lexical scoping)**という
* 静的有効範囲の有効な手続き中の自由変数は、外側の定義の束縛を指す（外側に向けて、手続きが定義された環境を探していく）
* 【以下参考】動的有効範囲(dynamic scoping)というのもある
 * ある関数内で定義された変数を、別の関数から参照できる（レキシカルな言語ではもちろんできない。変数名をスタックで管理しているとのこと）
 * 変数の値が、関数呼び出しのタイミングによって変わりうる。書きやすさと危険さの諸刃の剣
 * モダンな言語ではほとんど使われていない（最大ユーザーであったEmacslispでも、v24から惜しまれつつ引退。というのはウソで両方使えるようになったみたいです）