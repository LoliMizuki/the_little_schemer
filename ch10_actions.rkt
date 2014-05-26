#lang racket

(provide *const)
(provide *identifer)
(provide *lambda)

(require "ch01.rkt")
(require "ch07.rkt")
(require "ch10_entry_table.rkt")

;(define meaning
;  (lambda (e table)
;    ((expression-to-aciton e) e table)))

; dummy for meaning
(define meaning (lambda (e table) (#f)))

; = *const =
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? #t e) #t)
      ((eq? #f e) #f)
      (else (build (quote primitive) e)))))

; = *quote =
(define text-of (lambda (e) (second e)))
(define *quote (lambda (e table) (text-of e)))

; = *identifer =
(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *identifer 
  (lambda (e table)
    (lookup-in-table e table initial-table)))

; = *lambda =
; primitive: 
; non-primitive: 包含了 formal arguments 和 bodies, 記在 table 裡
(define *lambda
  (lambda (e table)
    (build (quote non-primitive) (cons table (cdr e)))))

; = *cond =
(define else? 
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of first)

(define answer-of second)

(define evcon ; every cond line
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines) table)))
      ((meaning (question-of (car lines) table)) (meaning (answer-of (car lines) table)))
      (else (evcon (cdr lines) table)))))

(define cond-lines-of cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

; test
; (*cond '(cond (coffee klatsch) (else party)) '(((coffee #t)) ((klatsch party) (5 (6)))))

; = *application =

; returns a list composed of the meaning of each argument
(define evlist
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else (cons (meaning (car args) table) (evlist (cdr args)))))))

(define function-of car)

(define arguments-of cdr)

; function 的種類 primitive 和 non-primitive
; 表現方式: 
;     (primitive primitive-name)
;     (non-primitive (table formals body)), 其中 (table formals body) 這個 list 被稱為 closure record

(define primitive? (lambda (l) (eq? (first l) (quote primitive))))

(define non-primitive? (lambda (l) (eq? (first l) (quote non-primitive))))

; atom? 的輔助
(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

; 執行原生函數
(define apply-primitive
  (lambda (name vals)
    (cond 
      ((eq? name 'cons) (cons (first vals) (second vals)))
      ((eq? name 'car) (car vals))
      ((eq? name 'cdr) (cdr (first vals)) ; *****
      ((eq? name 'null?) (null? (first vals)))
      ((eq? name 'eq?) (eq? (first vals) (second vals)))
      ((eq? name 'atom?) (:atom? (first vals))) ; 使用 :atom?
      ((eq? name 'zero?) (zero? (first vals)))
      ((eq? name 'add1) (add1 (first vals)))
      ((eq? name 'sub1) (sub1 (first vals)))
      ((eq? name 'number?) (number? (first vals)))))))

; 執行 closure
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closoure) 
             (extended-table (new-entry (formals-of closure) vals))
             (table-of closure))))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define *application
  (lambda (e table)
    (apply 
     (meaning (function-of e) table)
     (evlist (arguments-of e) table))))