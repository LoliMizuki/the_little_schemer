; 10. What is the Value of All of This?

#lang racket

(require "../ch01.rkt")
(require "ch10_actions.rkt")

; 求 e 的值
(define value
  (lambda (e)
    (meaning e '())))

; 求 e 的意義, 得出對應的 action 並執行
; - table: ?
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; == xxx-to-action ==

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

;(meaning '(lambda (x y) (cons x y)) '(((y z) ((8) 9))))
;(value 123)
;(value '(quote abc))
;(value '('(lambda (x y) (cons x y)) '(((y z) ((8) 9)))))

; == *cond ==

; Specail for *cond
; - 需要用上 meaning
; ...

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

; - lines: 每一條 cond-line 的集合
;  - ex: [((zero? x) (+ x x))
;         (... 二號)
;         (... 最可愛的三號)]
; - ev?
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? 'else x))
      (else #f))))

(define cond-lines-of cdr)
(define question-of first)
(define answer-of second)

; test *cond
;(define e '(cond (coffee klatsch) (else party)))
;(define table '(((coffee) (#t)) ((klatsch party) (5 (6)))))
;(*cond e table)

; 
(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
       (cons (meaning (car args) table) (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply-re
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

; Types of functions: "Pritimitives" and "Non-pritimitives"

(define pritimitive? (lambda (l) (eq? (first l) 'privtimitive)))
(define non-pritimitive? (lambda (l) (eq? (first l) 'non-privtimitive)))

; apply 已有內建
(define apply-re
  (lambda (func vals)
    (cond
      ((pritimitive? func) (apply-pritimitive (second func) vals))
      ((non-pritimitive? func) (apply-clouse (second func) vals)))))

; vals 的形式?
(define apply-pritimitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (fisrt vals) (second vals)))
      ((eq? name 'car) (car (fisrt vals)))
      ((eq? name 'cdr) (cdr (fisrt vals)))
      ((eq? name 'null?) (null? (first vals)))
      ((eq? name 'eq?) (eq? (fisrt vals) (second vals)))
      ((eq? name 'atom?) (:atom? (fisrt vals)))
      ((eq? name 'zero?) (zero? (fisrt vals)))
      ((eq? name 'add1) (add1 (fisrt vals)))
      ((eq? name 'sub1) (sub1 (fisrt vals)))
      ((eq? name 'number?) (number? (fisrt vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'pritimitive) #t) ;?
      ((eq? (car x) 'non-pritimitive) #t) ;?
      (else #f))))

;(define apply-closure
;  (lambda (closure vals)
;    ())