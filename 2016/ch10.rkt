; 10. What is the Value of All of This?

#lang racket

(require "ch01.rkt")
(require "ch09.rkt")

; entry 定義:
; 一個 pair
; (= (length fisrt) (length second)) => #t
; (set? fisrt) => #t

(define new-entry build)

; -f: 用來處理當無法得到結果時的 function, Miz 先叫他 fail function 吧 :D
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

;(lookup-in-entry-help )
;(lookup-in-entry-help 'f '(a b c) '(1 2 3) (lambda (n) #f))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

;(lookup-in-entry 'b '((a b c) (1 2 3)) (lambda (n) #f))

; table 的定義:
; a list of entries

; extend-table: 用以建構 entry 和 table
; 參數: new-entry table
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
                             ; 將繼續搜尋作為 lookup-in-entry 的 fail function
                             ; 處理搜尋目前的 entry 搜尋失敗
                             (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

;(lookup-in-table 'entree
;                 '(((entree dessert) (spaghetti spumoni))
;                   ((appetizer entree beverage) (food tastes good)))
;                 (lambda (name) (#f)))

; 幾種 type
; *const: 123, #t, #f
; *quote: 'Mizuki
; *identifier: Mizuki
; *lambda: (lambda (a b) (+ a b))
; *application: ((lambda (a b) (+ a b)) 11 99)
; *cond: (cond (nothing 'someyhing) (else #f))
; --
; primitive car/cons/cdr: car, cons, cdr

; Support
(define text-of second)
(define initial-table (lambda (name) (car '())))

; Type actions
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? #t e) #t)
      ((eq? #f e) #f)
      (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(define *application (lambda (e table) 'application))

(define *cond '*cond)

; atom to action
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

; list to action
; (aaa x y z) 
; => 若 aaa 是 atom => 判斷是 quote? lambda? cond?
; => 若非 atom => application
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
       (else *application))))

; 表達式的動作
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

; 
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; 求 e 值
(define value
  (lambda (e)
    (meaning e '())))

(meaning '(lambda (x y) (cons x y)) '(((y z) ((8) 9))))
; lambda 沒做出答案, 請檢查前面定義是否有誤