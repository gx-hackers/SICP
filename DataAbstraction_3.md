# 2.1 データを用いた抽象化の構築
## 2.2.2 階層構造

```Scheme
(cons (list 1 2) (list 3 4))
; => ((1 2) 3 4)
```

は下と同じ。

```Schme
(cons
      (cons 1 (cons 2 ()))
      (cons 3 (cons 4 ())))
; => ((1 2) 3 4)
; goshでnilは()
```

なんとなく直感的でない。

分解してみる。

```Scheme
(cons 1
      (cons 3 ()))
; => (1 3)

(cons 1 (list 3))
; => (1 3)
```

うんうんという感じ。

```Scheme
(cons (list 1) (list 3))
; => ((1) 3)

(cons (list 1) (list 3 4))
; => ((1) 3 4)
```

ということは下のようになる。

```Scheme
(cons (list 1 2) (list 3 4))
; => ((1 2) 3 4)
```

納得？

図で表すと以下。

![スクリーンショット 2014-04-05 19.55.11.png](https://qiita-image-store.s3.amazonaws.com/0/4194/1c1c051a-bd8c-4f6d-8d0c-d45a0c0ecbff.png "スクリーンショット 2014-04-05 19.55.11.png")

木構造で表すと次のようになる。

![スクリーンショット 2014-04-05 19.44.09.png](https://qiita-image-store.s3.amazonaws.com/0/4194/3df740c5-ff70-4f5d-895f-7d60435aaf72.png "スクリーンショット 2014-04-05 19.44.09.png")

再帰は操作を枝から枝へ、そして木の葉へと伝える。

2.2.1で登場したlengthで長さを求めると。

```Scheme
(define x (cons (list 1 2) (list 3 4)))
(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))
(length x)
; => 3
; ※ (1 2)と3と4の3つ

(length (list x x))
; => 2
; あくまでリストの個数を計算している
```

木の葉の数を数える`count-length`を比較する。

`length`が、`x`の`cdr`だけに注目していたのに対して、`count-length`は、

- `x`の`car`自身も数える対象となる

可能性があり、かつ、

- `x`の`car`の`count-length`と`cdr`の`count-length`の和

であると言える。

またこのとき、葉の`count-length`は`1`とする。

このとき`count-length`は、

```Scheme
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(count-leaves x)
; => 4

(count-leaves (list x x))
; => 8
```

### 木に渡るmap

mapの木対応を行う。

```Scheme
(define (scale-tree tree factor)
  (cond ((null? tree) ()) ; nullなら()
        ((not (pair? tree)) (* tree factor)) ; 対じゃないなら=葉ならfactor倍する
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
; => (10 (20 (30 40) 50) (60 70))
```

また、別の方法では木を部分木の列とみなしmapを使う方法。

```Scheme
(define (scale-tree tree factor)
  (map (lambda (sub-tree) ; sub-treeには(car tree)が入る
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
; => (10 (20 (30 40) 50) (60 70))
```

(・∀・)

## 2.2.3 慣習的インターフェイスとしての例
慣習的インターフェイス　== conventional interfaces

データ構造の設計原則、らしい。

まず以下の2つを比較してみる。

```Scheme
; あたら得られたtreeの葉が奇数のものの2乗の和
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
```

```Scheme
; 偶数からなるフィボナッチ数列でnよりも小さい値までのリスト
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
```

この2つの手続の抽象的な記述には大きな類似性がある。

- 木の葉を列挙する
- フィルタを通して奇数のみを選ぶ
- 選択された数の二乗を求める
- 初期値0にて`+`を用いて集積する。

2つ目のプログラムは

- 0からnを列挙する
- 各整数のフィボナッチ数を求める
- フィルタを通して偶数を選択する
- 初期値は空リストにて`cons`を用いて結果を集積する

図で表すと以下のようになる。

![スクリーンショット 2014-04-05 21.12.51.png](https://qiita-image-store.s3.amazonaws.com/0/4194/31a672fa-2d1a-7c59-a0a3-03700030854a.png "スクリーンショット 2014-04-05 21.12.51.png")
※ enumerator: 数を数える人,accumulate: 集積者

こんな感じで信号処理みたいに書けるじゃん？（だからなんだ）

### 列命令

さらに信号伝達構造っぽく考えることはできるか？
"信号"をリスト捉えてみる。

```Scheme
(map square (list 1 2 3 4 5))
; => (1 4 9 16 25)
```

増幅器みたいな？

フィルタリングもするには以下のようにする。

```Scheme
(define (filter predicate sequence) ; predicateがフィルタの条件
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
```

使ってみる。

```Scheme
(filter odd? (list 1 2 3 4 5))
; => (1 3 5)
```

集積は次のようにする。

```Scheme
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
; => 15

(accumulate * 1 (list 1 2 3 4 5))
; => 120

(accumulate cons nil (list 1 2 3 4 5))
; => (1 2 3 4 5)
```

ある区間の整数を列挙するには、

```Scheme
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
; => (2 3 4 5 6 7)
```

のようにし、木の葉を列挙するには、

```Scheme
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; => (1 2 3 4 5)
```

のようにする。

これらによって、先ほどの信号伝達図のように再形式化して書くことができる。


```Scheme
(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))
```

```Scheme
(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))
```

このような、伝達的プログラミングの価値は、モジュラデザインのプログラミングデザインを行うことを助けることだ。

以下複数の例が続く。
＜略＞

### 入れ子のmap
入れ子になっている例。
頭の体操にどうぞ。
＜略＞

## 2.2.4 例: ピクチャー言語

Win/Macの環境。

![9aa2DQV30L.gif](https://qiita-image-store.s3.amazonaws.com/0/4194/d248e2c7-e490-b3a2-6a78-ff0fef1dae05.gif "9aa2DQV30L.gif")

[waveソースコード](http://d.hatena.ne.jp/tetsu_miyagawa/20130512/1368350808)

```Scheme
#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define w1 (make-vect 0.00 0.85))
(define w2 (make-vect 0.15 0.62))
(define w3 (make-vect 0.30 0.70))
(define w4 (make-vect 0.42 0.70))
(define w5 (make-vect 0.38 0.88))
(define w6 (make-vect 0.40 1.00))
(define w7 (make-vect 0.62 1.00))
(define w8 (make-vect 0.65 0.88))
(define w9 (make-vect 0.60 0.70))
(define w10 (make-vect 0.75 0.70))
(define w11 (make-vect 1.00 0.38))
(define w12 (make-vect 1.00 0.15))
(define w13 (make-vect 0.64 0.48))
(define w14 (make-vect 0.78 0.00))
(define w15 (make-vect 0.62 0.00))
(define w16 (make-vect 0.52 0.30))
(define w17 (make-vect 0.40 0.00))
(define w18 (make-vect 0.25 0.00))
(define w19 (make-vect 0.36 0.52))
(define w20 (make-vect 0.30 0.64))
(define w21 (make-vect 0.15 0.43))
(define w22 (make-vect 0.00 0.67))
(define wave
  (segments->painter
   (list (make-segment w1 w2)
         (make-segment w2 w3)
         (make-segment w3 w4)
         (make-segment w4 w5)
         (make-segment w5 w6)
         (make-segment w7 w8)
         (make-segment w8 w9)
         (make-segment w9 w10)
         (make-segment w10 w11)
         (make-segment w12 w13)
         (make-segment w13 w14)
         (make-segment w15 w16)
         (make-segment w16 w17)
         (make-segment w18 w19)
         (make-segment w19 w20)
         (make-segment w20 w21)
         (make-segment w21 w22)
         )))
(paint wave)
```

![wave2.gif](https://qiita-image-store.s3.amazonaws.com/0/4194/3c6063b8-6a1d-917b-5e4e-896271248621.gif "wave2.gif")

```
(define einstein2 (beside einstein (flip-vert einstein)))
(define einstein4 (below einstein2 einstein2))
```

