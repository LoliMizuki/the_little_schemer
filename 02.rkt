#lang racket

(require "01.rkt")

(define plus 
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (add1 (plus n (sub1 m)))))))

(define minus
  (lambda (n m) 
    (cond
      ((zero? m) n)
      (else (sub1 (minus n (sub1 m))))))) 

(define addtup 
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))

; test
(addtup (list 1 2 3))