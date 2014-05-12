#lang racket

; ch02. Do it, Do it Again, and Again, and Again ...

(provide lat?)
(provide member?)



(require "ch01.rkt")



(define lat?
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


(define miz-lat? 
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((not (atom? (car l))) #f)
      (else (lat? (cdr l))))))

(define member? 
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(define miz-member? 
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))