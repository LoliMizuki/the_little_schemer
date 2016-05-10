; 10. What is the Value of All of This?

#lang racket

(require "../ch01.rkt")
(require "ch10_actions.rkt")

; 求 e 的意義, 得出對應的 action 並執行
; - table 的解釋? 
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; 求 e 的值
(define value
  (lambda (e)
    (meaning e '())))

;(meaning '(lambda (x y) (cons x y)) '(((y z) ((8) 9))))
;(value 123)
;(value '(quote abc))
;(value '('(lambda (x y) (cons x y)) '(((y z) ((8) 9)))))

; Specail for cond
; - 需要用上 meaning
; ...

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? 'else x))
      (else #f))))

(define question-of first)

(define answer-of second)

; - lines: 每一條 cond-line 的集合
;  - ex: [((zero? x) (+ x x))
;         (... 二號)
;         (... 最可愛的三號)]
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

; ** redefine *cond here **
(define *cond
  (lambda () (printf "not yet")))