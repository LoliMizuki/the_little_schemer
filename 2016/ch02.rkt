; 2. Do it, Do it Again, and Again, and Again ...

#lang racket

(require "ch01.rkt")

(provide lat?)
;(provide member?)

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      (else (or (eq? a (car l)) (member? a (cdr l)))))))
      