#lang racket

; ch01. Toys

(provide atom?)

(define atom? (lambda (a)  (not (list? a))))