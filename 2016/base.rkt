#lang racket

(provide atom?)

;(define atom?
;  (lambda (x)
;    (and (not (pair? x)) (not (null? x)))))

(define atom?
  (lambda (x) (not (list? x))))