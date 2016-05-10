; 10. What is the Value of All of This?
; Actions

#lang racket

(require "../ch01.rkt")
(require "ch10_utilties.rkt")

(provide *const)
(provide *quote)
(provide *identifier)
(provide *lambda)
(provide *application)
(provide *cond)

(provide atom-to-action)
(provide list-to-action)
(provide expression-to-action)

; 幾種 type
; *const: 123, #t, #f
; *quote: 'Mizuki
; *identifier: Mizuki
; *lambda: (lambda (a b) (+ a b))
; *application: ((lambda (a b) (+ a b)) 11 99)
; *cond: (cond (nothing 'someyhing) (else #f))
; --
; primitive car/cons/cdr: car, cons, cdr

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
