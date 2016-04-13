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

; TODO:
;(define multiinsertLR&co
;  (lambda (new oldL oldR lat col)
;    ()))


; col 定義, 若碰到 oldL => L += 1, oldR => R += 1