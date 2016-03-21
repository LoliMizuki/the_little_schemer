#lang racket

(provide lookup-in-table)

(require "ch07.rkt")

; entry: 一個 pair, 第一元素是 names(keys) 的集合, 第二是 values 的集合
; build entry, give a set of names and a list of values
(define new-entry build)

; == lookup-in-entry == 
; for lookup-in-entry
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

; table: a list of entries

; add entry into front of table
(define extended-table cons)

; == lookup-in-table ==
; table-f, 終止時執行的 func
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry 
             name
             (car table)
             (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

; test
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