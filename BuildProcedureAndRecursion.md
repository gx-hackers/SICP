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
    (sqrt-iter (improve guess x) x)))

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

user=> (square 3)
9

user=> (square (sqrt 1000))
1000.000369924366
```
* "for"などのループ構文がなくても、再起を利用して反復計算を実行できる

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