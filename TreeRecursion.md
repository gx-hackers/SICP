# 1.2.2 木構造再帰 (Tree Recursion)
原文: http://mitpress.mit.edu/sicp/full-text/sicp/book/node16.html
邦訳: http://sicp.iijlab.net/fulltext/x122.html#ss122

## 木構造再帰とFibonacci数列

　一般的にFibonacci数は:

  0, 1, 1, 2, 3, 5, 8, 13, 21, ...

の規則で定義出来る. Fibonacci数を計算する再帰的手続きの定義へ翻訳出来る:

```Clojure:fib.scm
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(print (fib 0)) ; 0
(print (fib 1)) ; 1
(print (fib 2)) ; 1
(print (fib 10)); 55

```

　この計算パターンを考える. 手続きfibは起動される度に自分を二回呼び出すという事実を反映している.

![木構造](http://gyazo.com/d575cbe6d9c6f5dcb1a6e6ecf18efb0f.png)

　　　　　　　　図1.5　(fib 5)の計算で生成された木構造再帰プロセス

　しかしこの手続きはFibonacci数の計算としては非効率.

- (fib 3)の計算が完全に重複
- (fib 1)や(fib 0)を計算する回数は,正確にFib(n + 1)回
    - Fib(n)の値がnに指数的に増加することを考えるとやばいのがわかる

　ただし必要なスペースは, 計算中どの節が上方に残っているかを覚えておくなら入力した数に対して線形にしか増加しない（木構造から一度計算したものを消していくとわかる）.

```
〜豆知識〜

より精密にはFib(n)はφ^2/√5に最も近い整数。ただし φ 黄金比(golden ratio)であり,

φ^2 = (1.618)^2 = 2.618 = φ + 1

を満す.
ちなみに√5は2.236なので、

2.618 / 2.236 = 1.171

なので確かに満たす。
```


## 反復的アルゴリズム
　Fibonacci数の計算の反復的プロセスを形式化することも出来る. 二つの整数a, bをFib(1)=1, Fib(0)=0で初期化し,

    a ← a+b
    b ← a

を同時の変換を繰り返す（1行目を実行したあとのaの値を2行目のaで使うわけではない）. n回の変更のあと, a, bはそれぞれFib(n+1), Fib(n)に等しい.

```Clojure:fib2.scm
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count) ; count数しか以下は実行されない
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
```

を使い, 反復的にFibonacci数を計算することが出来る.
これはnの増加に対して比例的にしか計算が増えないﾜｰｲヽ(ﾟ∀ﾟ)ﾒ(ﾟ∀ﾟ)ﾒ(ﾟ∀ﾟ)ﾉﾜｰｲ

　このことから木構造再帰は使いものにならないと決めてはいけない. 木構造再帰ずっと直截的であり, 反復的アルゴリズムの形式化には, 計算を三つの状態変数を使う反復へ作り替えられることに気がつく必要がある.

### 例: 両替の計算 〜反復アルゴリズムへの到達〜
問題

    50セント, 25セント, 10セント, 5 セント, 1セントを使って、1ドル(=100セント)を両替した場合に、その組合せは何通りあるか？(100セントを5種類の貨幣で両替する組合せの数)

>使える硬貨をある順に並べたとすると次の関係が成り立つ
>n種類の硬貨を使う, 金額aの両替の場合の数は:
>- 最初の種類の硬貨以外を使う, 金額aの両替の場合の数, 足す
>- dを最初の硬貨の額面金額[denomination]として, n種類の硬貨を使う, 金額a - dの両替の場合の数

なんのこっちゃ。

例を挙げると、


    100セントを5種類の貨幣で両替する組合せの数:
    50セントを使わない場合の4種の貨幣で100セントを両替した組合せの数と、
    100セント - 50セントしたときの金額50セントを5種の貨幣で両替したときの組合せの数の合計

ということみたい。さらに条件、

    ① 金額aがちょうど0なら, 両替の場合の数は1
    ② 金額aが0より少なければ, 両替の場合の数は0
    ③ 貨幣の種類nが0なら, 両替の場合の数は0


ソース

```Clojure:exchange.scm
; 最初に5番目の貨幣(50セント)を使う
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins) ; 金額と貨幣の種類数
  (cond ((= amount 0) 1) ; ①
        ((or (< amount 0) (= kinds-of-coins 0)) 0) ; ② or ③
        (else (+ (cc amount (- kinds-of-coins 1)) ; 金額aをn-1種の貨幣で両替
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))))) ; (金額a - 貨幣nの金額)をn種の貨幣で両替

; 貨幣の種類と金額の対応
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100) ; => 292
```

そのまま書き下せているのがわかる。

問題はまだ解いてないです＞＜

問題 1.11
> n<3に対してf(n)=n, n ≥ 3に対してf(n)=f(n - 1) + 2 f(n - 2) + 3 f(n - 3)なる規則で定義する関数fがある. 再帰的プロセスの方法でf を計算する手続きを書け. 反復的プロセスの方法でfを計算する手続きを書け.

[https://gist.github.com/kazuph/8971952](https://gist.github.com/kazuph/8971952)

問題 1.12

> 次の数のパターンを{\bf Pascal}三角形(Pascal's triangle)という.
三角形の辺上の数はすべて1, 三角形の内部の数はその上の二つの数の和である.4 再帰的プロセスの方法でPascal三角形の要素を計算する手続きを書け.

![http://gyazo.com/7b7bb22da2fafba4375c88e5bcffe3f1.png](http://gyazo.com/7b7bb22da2fafba4375c88e5bcffe3f1.png)
[http://d.hatena.ne.jp/tmurata/20090330/1238370771](http://d.hatena.ne.jp/tmurata/20090330/1238370771)
[http://community.schemewiki.org/?sicp-ex-1.12](http://community.schemewiki.org/?sicp-ex-1.12)
[http://sh1.2-d.jp/b/2007-07-14-02-03.html](http://sh1.2-d.jp/b/2007-07-14-02-03.html)

問題 1.13

> φ= (1+)/2としてFib}(n)がφn /に最も近い整数であることを証明せよ. ヒント: ψ= (1-)/2とする. 帰納法とFibonacci数の定義(1.2.2節参照)を用い, Fib}(n)=(φn-ψn)/を証明せよ.

う、証明、、、やんなくてもいいですよね、、、。

[http://d.hatena.ne.jp/yinkyweb/20100419/1271633999](http://d.hatena.ne.jp/yinkyweb/20100419/1271633999)
[http://ongaeshi.hatenablog.com/entry/20080223/1203755918](http://ongaeshi.hatenablog.com/entry/20080223/1203755918)
