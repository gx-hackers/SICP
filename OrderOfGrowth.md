# 1.2.3 増加の程度
計算量の話。
計算資源を消費する速度は、その計算プロセスによって大きく異なる。  
ここで言う計算資源とは、時間(時間効率)とメモリ(空間効率)だと思う。  
計算量は、R(n)で表現する。 ;; 最近の本だとO(ラージオー)では?

- n    : 問題の大きさを表すパラメタ
- R(n) : 計算に必要な資源の量

nは場合によって変わる。  

- 平方根の近似値計算なら、nは要求する制度の桁数
- 行列の計算なら、nは行列の行の数

R(n)も場合によって変わる。  

- メモリ
- 実行した基本機械演算(ANDとかNOTとか?)

十分な大きさのnに対し、  

  k1f(n) <= R(n) <= k2f(n)  

となるnと独立な定数k1, k2があれば、R(n)は Theta(f(n)) の増加の程度だという。  
R(n) = Theta(f(n)) である。  
計算の性質によって、必要な計算資源を見積もることができる。  

- 階乗再帰計算なら、ステップ数、メモリはTheta(n)で増加
- 反復的階乗計算では、ステップ数はTheta(n)、メモリはTheta(1)で増加

上の例だと、反復的プロセスだと空間効率が良くてお得。  
その他の見積り例は以下。

- n^2 も 2n^2 + 5n + 10も、Theta(n^2)
- 定数 < log n < n < n log n < n^2 < n^2 < 2^n < 3^n < n!
- 定数だと計算量はスケールしなくてgood
- 次数が大きくなるとヤバイ

# 1.2.4 べき乗

- 基本手続きremember
- 計算量を考える実例
- 不変量
- 教科書を読もう!

# 1.2.5 最大公約数

- アルゴリズム
- 時間があれば取り組む
- 時間がなかったのでパスで

# 1.3 高階手続きによる抽象

# 1.3.1 引数としての手続き
以下の3つの関数はなにか似ている

```sum-integers
(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))
```

```sum-cubes
(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))
```

```pi-sum
(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 4) b))))
```

似ているので抽象化できる

```abstract
(define (<name> a b)
  (if (> a b)
    0
    (+ (<term> a)
       (<name> (<next> a) b))))
```

ちゃんと書くとこうなる

```abstraction
(define (sum term a next b)
  (if (> a b)
    0
    (+ term (next a) next b))))
```

ここで、aとbは上限・下限の数値、termとnextは手続き。  
この抽象化を使うと、最初の3関数はいい感じに書ける。

```sum-cubes
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum sube a inc b))
(sum-cubes 1 10) ; 3025
```

```sum-integers
(define (identity x) x)
(define (sum-integers a b)
  (sum identitiy a inc b))
(sum-integers 1 10) ; 55
```

```pi-sum
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
(define (pi-next x)
  (+ x 4))
(sum pi-term a pi-next b)
(* 8 (pi-sum 1 1000)) ; 3.139...
```

# 1.3.2 lambdaを使う手続きの構築
## 無名手続きlambda
フォーマットは以下。

```
(lambda (<parameters>) <body>)
```

例えば、  

```
((lambda (x y z) (+ x y (square z))) 1 2 3) ; 12
```

と呼べる。  

## 無名変数let
lambdaの糖衣構文。  
RSpecだとletは遅延評価なのでビクッとなる。

```format
(let ((<var1> <exp1>)
      (<var2> <exp2>)
      ...
  <body>)
```

実際の使用例は以下。

```
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
```

これをlambdaで書くと  

```
(define (f x y)
  ((lambda (a b)
      (+ (* x (square a))
         (* y b)
         (* a b)))
    (+ 1 (* x y))
    (- 1 y)))
```

letの方が見やすい。

# 1.3.3 一般的手法としての手続き
問題多い

# 1.3.4 値として返される手続き

- 手続きを返す手続きの話
- 抽象と第一級手続き
