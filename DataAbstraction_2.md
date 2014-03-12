# 2.2 階層データ構造と閉包性

* (**(cons 1 2)**で作った)対は基本的な「糊」
* 箱とポインタ記法(box-and-pointer notation)：対は2連の箱で、carへのポインタと、cdrへのポインタがある。図2.2
* consは(基本オブジェクトである)数値だけでなく、対さえも組み合わせることができる。従って対はあらゆる種類のデータ構造を構成するための万能構成部品

consの閉包性(closure property)とは、要素が対であるような対を作る能力。よって階層的(hierarchical)構造を作ることができる

# 2.2.1 並びの表現

図2.4＝鎖状の対。それぞれの対のcarが対応する項で、cdrが鎖での次の対。最後の対のcdrは図では斜線。プログラムでは変数nil。対でない特別の値を指すことで並びの終わりを示す。

## リスト

入れ子のconsで作られた対の並びをリスト(list)という。
Lispシステムはリストを印字する時は便宜的にかっこで囲まれた要素の並びを印字する。

```Scheme
gosh> (define a1 (cons 1 (cons 2 (cons 3 (cons 4 () )))))
a1
gosh> a1
(1 2 3 4)

gosh> (define a2 (list 1 2 3 4))
a2
gosh> a2
(1 2 3 4)
```

式(list 1 2 3 4)と,その式を評価して得たリスト(1 2 3 4)を混同しないよう注意しよう。
式(1 2 3 4)を評価しようとすると、解釈系が手続き1を引数2と3と4に作用しようとしたというエラーになる。

carはリストの最初の項を選択し,cdrは先頭の項以外からなる部分リストを選択すると考えてよい

```Scheme
gosh> (car a1)
1
gosh> (car a2)
1
gosh> (cdr a1)
(2 3 4)
gosh> (cdr a2)
(2 3 4)
```

carとcdrの入れ子の作用はリストの二番目、三番目、それに続く項を取り出すのに使う。＝書くのが煩わしいので、Lispの方言はその省略形を用意している 例えば

```Scheme
gosh> (car (cdr a1))
2
gosh> (cadr a1)
2
```

これが要は2番目を取りたい時。3番目以降は本質的にcadrの入れ子で書くべき？

nilは、nihilの短縮形。

## リスト演算

「cdrダウン」でリストを操作する、がコンセプト。以下はいくつかのパターン

list-refは、リストと数値nをとり、リストのn番目のものを返す。
* n=0 なら、list-refはリストのcar
* そうでなければ、リストのcdrの(n-1)番目を返す
* →itemsをcdrしながら(減らしながら)再帰

```Scheme
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)
16
```

lengthは
* 任意のリストのlengthはそのリストのcdrのlength＋1である
* →リスト全体をcdrダウン

```Scheme
(define (length items)
  (if (null? items)
    0
   (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(length odds)
4
```

一方で、lengthは反復でも表現できる

```Scheme
(define (length items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
```

appendのように、リストをcdrダウンしつつ、consアップするものもある。再帰的に実装するなら
* list1が空リストなら、結果は単にlist2である
* そうでなければ、list1のcdrとlist2をappendし、その結果にlist1のcarをconsする
* (再帰を考える時に、この終了条件を先に考えるのはアリだなぁ)

```Scheme
(define (append list1 list2)
  (if (null? list1)
  list2
  (cons (car list1) (append (cdr list1) list2))))

(append squares odds)
```






