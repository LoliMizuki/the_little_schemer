#lang racket

; ch.10 What Is the Value of All of This?

(require "ch01.rkt")
(require "ch07.rkt")
(require "ch10_entry_table.rkt")
(require "ch10_actions.rkt")

; ====
; quote 在 racket 中, 可以換為
; (quote (a b c)) = '(a b c)

; type of car => *const
; (value car) => primitive car
;
; type of (quote nothing) => *quote
;
; types:
; *const: number, #t, #f
; *quote: (quote nothing)
; *identifier: nothing
; *lambda: (lambda ...)
; *cond: (cond ... ...)
; *application: ((lambda (x) (...)) 123); racket 中似乎是叫 #<procedure>
;
; (ex) 
; e = (lambda (x) (cons x y))
; table = (((y z) ((8) 9)))
; (meaning e table) => 
;         (non-primitive
;                 (((y z) ((8) 9))) --> table
;                 (x)               --> formal arguments 
;                 (cons x y)        --> body

(define table-of first)
(define formals-of second)
(define body-of third)

; atom-to-action
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else '*identifier))))

; list-to-action
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e)) 
       (cond
         ((eq? (car e) (quote quote)) '*quote)
         ((eq? (car e) (quote lambda)) '*lambda)
         ((eq? (car e) (quote cond)) '*cond)
         (else '*application)))
      (else '*application))))

; expression-to-action
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

;(define value
;  (lambda (e)
;    (meaning e (quote ()))))