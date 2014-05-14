#lang racket

; Ch.8 Lambda the Ultimate

(require "ch01.rkt")
(require "ch04.rkt")
(require "ch05.rkt")

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? a (car l)) (rember-f test? a (cdr l)))
      (else (cons (car l) (rember-f test? a (cdr l)))))))

;(rember-f eq? 'a (list 'c 'b 'a 'g))
;(rember-f = 5 '(7 3 5 6 8))
;(rember-f equal? (list 'pop 'corn)
;          (list 'lemonade (list 'pop 'corn) 'and (list 'cake)))

(define eq-c?
  (lambda (a)
    (lambda (x)
      (eq? a x))))

; ((eq-c? 'neko) 'neko)

; rember-f with lambda
(define rember-f-lambda
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? a (car l)) ((rember-f-lambda test?) a (cdr l)))
        (else (cons (car l) ((rember-f-lambda test?) a (cdr l))))))))

;((rember-f-lambda eq?) 'a (list 'c 'b 'a 'g))
;((rember-f-lambda eq?) 'eq? (list 'equal? 'eq? 'eqan? 'eqlist? 'eqpair?))

; no need to define this ... :D
(define rember-f-eq? (rember-f-lambda eq?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? old (car l)) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

;((insertL-f eq?) 'h 'a (list 'c 'b 'a 'g))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? old (car l)) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

;((insertR-f eq?) 'h 'a (list 'c 'b 'a 'g))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? old (car l)) (seq new old (cdr l)))
        (else (cons old ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g (lambda (new old l) (cons new (cons old l)))))
(define insertR (insert-g (lambda (new old l) (cons old (cons new l)))))
(define subst (insert-g (lambda (new old l) (cons new l))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) +)
      ((eq? x (quote *)) *)
      ((eq? x (quote ^)) expt))))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp)) 
             (1st-sub-exp nexp) (2nd-sub-exp nexp))))))

;(value '(1 + 2))
;(value '(1 * 2))
;(value '(3 ^ 2))

(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) ((multirember-f test?) a (cdr l)))
        (else (cons (car l) ((multirember-f test?) a (cdr l))))))))

;((multirember-f <) 5 '(1 2 3 4 5 6 7 8 9 10))
;((multirember-f eq?) 'tuna (list 'shrimp 'salad 'tuna 'salad 'and 'tuna))

(define multirember-eq? (multirember-f eq?))

;(multirember-eq? 5 '(1 2 3 4 5 6 7 8 9 10))

; 直接將條件函數作為參數

(define multirember-T
  (lambda (test? lat)
    (cond
      ((null? lat) (quote ()))
      ((test? (car lat)) (multirember-T test? (cdr lat)))
      (else (cons (car lat) (multirember-T test? (cdr lat)))))))

;(multirember-T (lambda (a) (eq? '5 a)) '(1 2 5 4 5 6 7 8 5 10))

; col means collector or continuation
; 當 (eq? a lat) 成立時, 執行 col 

(define multirember&col
  (lambda (a lat col)
    (cond
      ((null? lat) (col (quote ()) (quote()))) ; 執行 col 的終止條件
      ((eq? (car lat) a) (multirember&col a (cdr lat) (lambda (newlat seem) ; 當不為終止時, 使用一個 functions 來記憶暫時的值s
                                                        (col newlat (cons (car lat) seem)))))
      (else (multirember&col a (cdr lat) (lambda (newlat seem)
                                           (col (cons (car lat) newlat) seem)))))))

; col 函數
(define a-friend (lambda (x y) (null? y)))

; 追蹤測試: 若 a: tuna, lat: ('tuna), col: a-friend
; -- 1st 輪 --
; (eq? (car lat) a) is TRUE => 執行: (multirember&col a (cdr lat) new-friend
; new-friend 的定義鐘包含了目前需要記錄的值
; 即 (define new-friend (lambda (newlat seem)
;                               (col newlat (cons 'tuna seem)))) # 其中 col 為 a-friend
; 
; -- 2nd 輪 --
; (null? lat) is TRUE => (col '() '()) => (new-friend '():newlat '():seem) 
; => (col '() (cons 'tuna '())) => (col '() '(tuna)) => (a-friend '() '(tuna)) => #f
;
; -- 結論 -- 
; 把符合條件存在 seem, 不符合的存在 newlat (這樣解釋 ok 嗎???
 
; test
;(multirember&col 'tuna (list 'strawberries 'tuna 'and 'swordfish) a-friend)

(define last-friend (lambda (x y) (length x)))

; (multirember&col 'tuna (list 'strawberries 'tuna 'and 'swordfish) last-friend)


