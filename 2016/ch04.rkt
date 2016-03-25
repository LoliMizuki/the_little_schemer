; 4. Numbers Games

#lang racket

(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

; (add 123 789)

(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (sub n (sub1 m)))))))

; (sub 999 777)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (addtup (cdr tup)))))))

; (addtup '(1 2 3 4))

(define multiply
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (multiply n (sub1 m)))))))

; (multiply 12 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (add (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;(tup+ '(1 8 1 2) '(2 0 1 6))
;(tup+ '(1 8 1 2 5 6 3 4) '(2 0 1 6))
;(tup+ '(1 8 1 2) '(2 0 1 6 5 4 3 2))

; >
(define morethan
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (morethan (sub1 n) (sub1 m))))))

(morethan 3 4) ; f
(morethan 4 3) ; t
(morethan 4 4) ; f

; <
(define lessthan
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lessthan (sub1 n) (sub1 m))))))

;(lessthan 3 4) ; t
;(lessthan 4 3) ; f
;(lessthan 4 4) ; f
