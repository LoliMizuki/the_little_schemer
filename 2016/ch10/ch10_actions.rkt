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
;(provide *cond) ; *cond 定義 => ch10 本文 (因為需要使用到 meaning)

; 幾種 type
; *const: 123, #t, #f
; *quote: 'Mizuki
; *identifier: Mizuki
; *lambda: (lambda (a b) (+ a b))
; *application: ((lambda (a b) (+ a b)) 11 99)
; *cond: (cond (nothing 'something) (else #f))
; --
; primitive car/cons/cdr: car, cons, cdr

; action 的 common form: (e table)

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
; lambda 動作解:
; 求 (meaning e table)
; => e is (lambda (x) (cons x y))
; => table is (((y z) ((8) 9)))
;
; Ans:
; (((y z) ((8) 9)))   (x)     (cons x y)
; ~~~~~~~~~~~~~~~~~   ~~~     ~~~~~~~~~~
; table               fomals  body
; 所以追加 support functions: table-of, formals-of, body-of

(define *application (lambda (e table) 'application))

; *cond 在 ch10 本文中