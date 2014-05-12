#lang racket

; Ch.8 Lambda the Ultimate

(require "ch05-1.rkt")

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? a (car l)) (rember-f test? a (cdr l)))
      (else (cons (car l) (rember-f test? a (cdr l)))))))

;(rember-f eq? 'a (list 'c 'b 'a 'g))
;(rember-f = 5 '(7 3 5 6 8))
;(rember-f equal? (list 'pop 'corn)
;          (list 'lemonade (list 'pop 'corn) 'and (list 'cake)))

(define eq-c?
  (lambda (a)
    (lambda (x)
      (eq? a x))))

; ((eq-c? 'neko) 'neko)

; rember-f with lambda
(define rember-f-lambda
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? a (car l)) ((rember-f-lambda test?) a (cdr l)))
        (else (cons (car l) ((rember-f-lambda test?) a (cdr l))))))))

;((rember-f-lambda eq?) 'a (list 'c 'b 'a 'g))
;((rember-f-lambda eq?) 'eq? (list 'equal? 'eq? 'eqan? 'eqlist? 'eqpair?))

; no need to define this ... :D
(define rember-f-eq? (rember-f-lambda eq?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? old (car l)) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

;((insertL-f eq?) 'h 'a (list 'c 'b 'a 'g))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? old (car l)) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

;((insertR-f eq?) 'h 'a (list 'c 'b 'a 'g))