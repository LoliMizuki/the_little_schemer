#lang racket

; ch04. Numbers Games

(provide plus)
(provide minus)
(provide addtup)
(provide multiply)
(provide divide)
(provide tup+)
(provide greater?)
(provide less? )
;(provide equal?)
;(provide equal2?)
(provide pow)

; as +
(define plus 
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (add1 (plus n (sub1 m)))))))

; as -
(define minus
  (lambda (n m) 
    (cond
      ((zero? m) n)
      (else (sub1 (minus n (sub1 m))))))) 

; (addtup (1 2 3))
(define addtup 
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))

; as *
(define multiply
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (multiply n (sub1 m)))))))

; as /
(define divide
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (divide (- n m) m))))))

; (tup+ (1 2 3) (4 5 6)) => (5 7 9)
(define tup+ 
  (lambda (tup1 tup2)
    (cond
      ; 因為下面的兩個條件, 這段不需要了
      ;((and (null? tup1) (null? tup2)) (quote ()))
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (plus (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

; (> 12 123) > #f, (> 123 12) > #t
(define greater?
  (lambda (m n) 
    (cond 
      ((eq? m n) #f)
      ((zero? m) #t)
      ((zero? n) #f)
      (else (greater? (sub1 m) (sub1 n))))))

; <
(define less? 
  (lambda (m n) 
    (cond 
      ((eq? m n) #f)
      ((zero? m) #f)
      ((zero? n) #t)
      (else (less? (sub1 m) (sub1 n))))))

; =, 第一種寫法
(define equal?
  (lambda (m n) 
    (cond
      ((zero? m) (zero? n))
      (else (equal? (sub1 m) (sub1 n))))))

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
(define occur; length of list
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
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
      (eqan? 1 n)))
; =, 使用 >, <
(define equal2?
  (lambda (m n) 
    (cond
      ((or (greater? m n) (less? m n)) #f)
      (else #t))))

; m^n
(define pow
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (multiply m (pow m (sub1 n)))))))

; test
;(addtup (list 1 2 3))
;(tup+ (list 1 2 3 4) (list 5 6 7 8))
;(tup+ (list 1 2 3 4) (list 5 6))
;">"
;(greater? 123 12)
;(greater? 12 123)
;(greater? 12 12)
;"<"
;(less? 123 12)
;(less? 12 123)
;(less? 12 12)
;"=(1)"
;(equal? 12 13)
;(equal? 13 13)
;"=(2)"
;(equal2? 12 13)
;(equal2? 13 13)