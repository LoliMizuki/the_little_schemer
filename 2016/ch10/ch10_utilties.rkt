; 10. What is the Value of All of This?
; Support Functions

#lang racket

(require "../ch01.rkt")
(require "../ch09.rkt")

(provide new-entry build)
(provide lookup-in-entry)
(provide extend-table)
(provide lookup-in-table)
(provide text-of)
(provide initial-table)

; entry 定義:
; 一個 pair
; (= (length fisrt) (length second)) => #t
; (set? fisrt) => #t

(define new-entry build)

; -f: 用來處理當無法得到結果時的 function, Miz 先叫他 fail function 吧 :D
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

; extend-table: 用以建構 entry 和 table
; 參數: new-entry table
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
                             ; 將繼續搜尋作為 lookup-in-entry 的 fail function
                             ; 處理搜尋目前的 entry 搜尋失敗
                             (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

;(lookup-in-table 'entree
;                 '(((entree dessert) (spaghetti spumoni))
;                   ((appetizer entree beverage) (food tastes good)))
;                 (lambda (name) (#f)))

; Support
(define text-of second)
(define initial-table (lambda (name) (car '())))