; 4. Numbers Games

#lang racket

(require "ch01.rkt")

(provide eqan?)
(provide ^)

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

;(morethan 3 4) ; f
;(morethan 4 3) ; t
;(morethan 4 4) ; f

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

; Miz 解
(define equal
  (lambda (n m) (zero? (sub n m))))

; 1
;(define equal
;  (lambda (m n)
;    (cond
;      ((zero? m) (zero? n))
;      ((zero? n) #f)
;      (else (eqaul (sub1 m) (sub1 n))))))

; rewrite use >, <
;(define equal
;  (lambda (m n)
;    (cond
;      ((or (morethan m n) (lessthan m n)) #f)
;      (else #t))))

;(equal 12 13)
;(equal 12 12)
;(equal 0 0)

; 次方, power of
(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (multiply n (^ n (sub1 m)))))))

;(^ 2 3)
;(^ 4 5)
;(^ 6 0)

(define division
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (division (sub n m) m))))))

;(division 6 3)
;(division 4 3)
;(division 0 3)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

;(length '(1 2 3))
;(length '())

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;(pick 3 '(4 5 6 7 8 9 2))

; 後面有重寫
;(define rempick
;  (lambda (n lat)
;    (cond
;      ((zero? (sub1 n)) (cdr lat))
;      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;(rempick 3 '(4 5 6 7 8 9 2))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat)) (no-nums (cdr lat)))
         (else (cons (car lat) (no-nums (cdr lat)))))))))

;(no-nums '(tt 3 yy 6 ee rr 8))
;(no-nums '(0 1 3 dd 4 r f g 777))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

;(all-nums '(tt 3 yy 6 ee rr 8))
;(all-nums '(0 1 3 dd 4 r f g 777))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (equal a1 a2))
      ((and (atom? a1) (atom? a2)) (eq? a1 a2))
      (else #f))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
        ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat))))))))

;(occur 'a '(a 1 b 2 a c a 3 a))

(define one?
  (lambda (n) (equal n 1)))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;(rempick 3 '(4 5 6 7 8 9 2))