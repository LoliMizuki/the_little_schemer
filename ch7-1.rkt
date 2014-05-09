#lang racket

(require "01.rkt")

; is lat a set?
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat)))))) 

;(set? (list 'a 'b 'c))
;(set? (list 'a 'b 'c 'a))
;(set? (list 'apple 3 'pear 4 9 'apple 3 4))
;(set? (list 1 2 3))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

; use multirember
(define makeset-ex
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons 
             (car lat)
             (makeset-ex (multirember (car lat) (cdr lat))))))))

; test
;(makeset-ex (list 'a 'b 'c 'd 'a))
;(makeset-ex (list 'apple 3 'pear 4 9 'apple 3 4))

; is set1 subset of set2?
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))
      
; test
(subset?
 (list 5 'chicken 'wings)
 (list 5 'hamburgers 2 'pieces 'fried 'chicken 'and 'light 'duckling 'wings))

(subset? 
 (list 4 'pounds 'of 'horseradish)
 (list 'four 'pounds 'chicken 'and '5 'ounces 'horseradish))


