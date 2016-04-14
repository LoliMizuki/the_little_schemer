; 8. Lambda the Ultimate

#lang Racket

(require "ch01.rkt")
(require "ch04.rkt")
(require "ch05.rkt")

;(define rember-f
;  (lambda (test? a l)
;    (cond
;      ((null? l) '())
;      ((test? a (car l)) (rember-f test? a (cdr l)))
;      (else (cons (car l) (rember-f test? a (cdr l)))))))

;(rember-f = 5 '(6 2 5 3))
;(rember-f eq? 'jelly '(jelly beans are good))
;(rember-f equal? '(pop corn) '(jelly beans are (pop corn) good))

; c: Currying です
(define eq?-c
  (lambda (a) (lambda (x) (eq? a x))))

(define eq?-salad (eq?-c 'salad))

;(eq?-salad 'salad)
;(eq?-salad 'tuna)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) ((rember-f test?) a (cdr l)))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))
;(rember-eq? 'jelly '(jelly beans are good))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons new (cons old ((insertL-f test?) new old (cdr l)))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons old (cons new ((insertR-f test?) new old (cdr l)))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l) (cons new (cons old l))))

(define seqR
  (lambda (new old l) (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old ((insert-g seq) new old (cdr l))))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

;(define insertL (insert-g seqL))
(define insertL (insert-g (lambda (new old l) (cons new (cons old l)))))
;(insertL 'z 'a '(c v a d g))

;(define insertR (insert-g seqR))
(define insertR (insert-g (lambda (new old l) (cons old (cons new l)))))
;(insertR 'z 'a '(c v a d g))

(define subst (insert-g (lambda (new old l) (cons new l))))

(define seqrem (lambda (new old l) l))
(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l))) ; #f 為原本 (insert-g new old l) 中的 new

;(rember 'sausage '(pizza with sausage and bacon))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else ^))))

(define operator (lambda (exp) (car exp)))
(define 1st-sub-exp (lambda (exp) (car (cdr exp))))
(define 2nd-sub-exp (lambda (exp) (car (cdr (cdr exp)))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))

;(value '(+ 123 333))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

;((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c 'tuna))
;(eq?-tuna 'tuna)
;(eq?-tuna 'nottuna)

(define multirember-T
  (lambda (test?)
    (lambda (lat)
      (cond
        ((null? lat) '())
        ((test? (car lat)) ((multirember-T test?) (cdr lat)))
        (else (cons (car lat) ((multirember-T test?) (cdr lat))))))))

;((multirember-T eq?-tuna) '(shrimp salad tuna salad and tuna))


; col 表示 collector function
; 用以表示 continuation(延續性, https://zh.wikipedia.org/wiki/%E5%BB%B6%E7%BA%8C%E6%80%A7)
; 把執行狀態傳遞下去
; 先不要由原 func name 去思考意義

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat) (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
      (else
       (multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y) (null? y)))
  
;(multirember&col 'tuna '() a-friend)
; [Analysis]
;   (null? lat) => (a-friend '() '()) => #t

;(multirember&co 'tuna '(tuna) a-friend)
; *Action 分析*
; 每次碰上 (eq? a (car lat)) 時 => 生成一個新的函數作為新的 col 參數
; 新 col => (cons (car lat) 參數2) => 將 參數2 與 (car lat) (即 a), cons 起來
; lat 走訪完畢後 => (col '() '()) 判斷 => 完成搜尋到的 a list
; a-friend 定義: (null? list)
; (感覺上還是看不懂 XD)

;(multirember&col 'tuna '(and tuna) a-friend) ; => 最後結果 '(tuna), 所以 #t
;(multirember&col 'tuna '(and notuna) a-friend) ; => 最後結果 '(), 所以 #f

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL) (cons new (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons (car lat) (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

;(multiinsertLR 'new '<- '-> '(A G <- H U Y -> R D D <-))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat)
                                              ; Here comes new collector
                                              (lambda (newlat L R)
                                                (col
                                                 (cons new (cons (car lat) newlat)) (add1 L) R))))
      ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat)
                                              ; Here comes new collector
                                              (lambda (newlat L R)
                                                (col
                                                 (cons (car lat) (cons new newlat)) L (add1 R)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat)
                              (lambda (newlat L R)
                                (col
                                 (cons (car lat) newlat) L R)))))))

; col 定義, 若碰到 oldL => L += 1, oldR => R += 1
(define look-lat (lambda (lat countL countR) lat))
(define look-countL (lambda (lat countL countR) countL))
(define look-countR (lambda (lat countL countR) countR))

(define test-lat '(A G <- H U Y -> R D D <-))

;(multiinsertLR&co 'new '<- '-> test-lat look-lat)
;(multiinsertLR&co 'new '<- '-> test-lat look-countL)
;(multiinsertLR&co 'new '<- '-> test-lat look-countR)

;(define even? (lambda (n) (= (* (/ n 2) 2) n))) racket of (/ n 2) is not int
(define even? (lambda (n) (= (modulo n 2) 0)))

(define even-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (even-only* (cdr l))))
         (else (even-only* (cdr l)))))
      (else (cons (even-only* (car l)) (even-only* (cdr l)))))))

;(even-only* '(1 2 3 4 5 6 7 8 9 0))
;(even-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l)) (evens-only*&co (cdr l)
                                         (lambda (newl p s) (col (cons (car l) newl) (* (car l) p) s))))
         (else (evens-only*&co (cdr l) (lambda (newlat p s) (col newlat p (+ (car l) s)))))))
      (else (evens-only*&co (car l) (lambda (al ap as)
                                     (evens-only*&co (cdr l) (lambda (dl dp ds) 
                                                              (col (cons al dl)
                                                                   (* ap dp)
                                                                   (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

; Miz 的嘗試: 將判斷和執行分離
; 函數只將 odd/even 分離
; 實際動作由傳入的 collector 定義
; 目前只能處理純粹的 atom-list ><"
(define even-odd-action&co
  (lambda (l col)
    (cond
      ((null? l) (col '() '()))
      ((atom? (car l))
       (cond
         ((even? (car l)) (even-odd-action&co (cdr l) (lambda (odd-l even-l) (col odd-l (cons (car l) even-l)))))
         (else (even-odd-action&co (cdr l) (lambda (odd-l even-l) (col (cons (car l) odd-l) even-l))))))
      (else #f)))) ; I can not handle ... >"<
             

; 定義動作
(define sum-of-even
  (lambda (odd-l even-l)
    (cond
      ((null? even-l) 0)
      (else (+ (car even-l) (sum-of-even odd-l (cdr even-l)))))))

(define sum-of-odd
  (lambda (odd-l even-l)
    (cond
      ((null? odd-l) 0)
      (else (+ (car odd-l) (sum-of-odd (cdr odd-l) even-l))))))

;(even-odd-action&co '(1 2 3 4 5 6 7) sum-of-even)
;(even-odd-action&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) sum-of-odd) => 不工作
