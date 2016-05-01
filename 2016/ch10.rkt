; 10. What is the Value of All of This?

#lang racket

(require "ch01.rkt")
(require "ch09.rkt")

; entry 定義:
; 一個 pair
; (= (length fisrt) (length second)) => #t
; (set? fisrt) => #t

(define new-entry build)

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
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table) (lambda (name)
                                                (lookup-in-table name (cdr table) table-f)))))))

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
    (build 'non-'primitive (cons table (cdr e)))))

(define *application (lambda (e table) ('application)))

(define *cond '*cond)

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
      ((eq? e 'number) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? e 'quote) *quote)
         ((eq? e 'lambda) *lambda)
         ((eq? e 'cond) *cond)
         (else *application)))
       (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) table)))

(define value
  (lambda (e)
    (meaning e '())))


;(meaning '(lambda (x y) (cons x y)) '(((y z) ((8) 9))))
; lambda 沒做出答案, 請檢查前面定義是否有誤

