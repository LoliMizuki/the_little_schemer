#lang racket

; ch.10 What Is the Value of All of This?

(require "ch01.rkt")
(require "ch07.rkt")

; entry: 一個 pair, 第一元素是 names(keys) 的集合, 第二是 values 的集合

; 建置 entry
(define new-entry build)

; for lookup-in-entry
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

;
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

; table: a list of entries

; 將 entry 加入 table 的前端
(define extended-table cons)

; table-f, 終止時執行的 func
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry 
             name
             (car table)
             (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

;(lookup-in-table 'entree
;                 (list (list (list 'entree 'dessert)
;                             (list 'spaghetti 'spumoni))
;                       (list (list 'appetizer 'entree 'beverage)
;                             (list 'food 'tastes 'good)))
;                 (lambda (name) #f))

;(lookup-in-table 'entree
;                 '(((entree dessert)
;                    (spaghetti spumoni))
;                   ((appetizer entree beverage)
;                    (food tastes good)))
;                 (lambda (name) #f))

; ====
; quote 在 racket 中, 可以換為 '
; (quote (a b c)) = '(a b c)

; car 的 type: primitive car
; 
; types:
; *const: number, #t, #f
; *quote: (quote nothing)
; *identifier: nothing
; *lambda: (lambda ...)
; *cond: (cond ... ...)
; *application: ((lambda (x) (...)) 123); racket 中似乎是叫 #<procedure>

; == actions ==

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

; = *cond =
(define else? 
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of first)

(define answer-of second)

(define evcon ; 傳入 cond 的每一行
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

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

;(define meaning
;  (lambda (e table)
;    ((expression-to-aciton e) e table)))

;(define value
;  (lambda (e)
;    (meaning e (quote ()))))