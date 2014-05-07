#lang racket

(require "01.rkt")
(require "02.rkt")

(provide length)
(provide pick)
(provide rempick)
(provide no-nums)
(provide all-nums)
(provide eqan?)
(provide occur)
(provide one?)

; length of list
(define length 
  (lambda (lat)
    (cond 
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

; pick n-th from a list
(define pick
  (lambda (n lat) 
    (cond 
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

; get a list removed n-th 
(define rempick
  (lambda (n lat)
    (cond 
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

; get a list of all non-numbers in origin list
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

; get a tups of all numbers in origin list
(define all-nums
  (lambda (lat)
    (cond 
      ((null? lat) (quote ()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

; is atom a1 equal atom a2?. a1 a2 maybe number or not
; (eq? can do that)
(define eqan?
  (lambda (a1 a2)
    (cond 
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

; count atom 'a' in 'lat'
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
      (eqan? 1 n)))