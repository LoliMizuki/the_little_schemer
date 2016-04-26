#lang racket

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

(lookup-in-table 'entree
                 '(((entree dessert) (spaghetti spumoni))
                   ((appetizer entree beverage) (food tastes good)))
                 (lambda (name) (#f)))
      

