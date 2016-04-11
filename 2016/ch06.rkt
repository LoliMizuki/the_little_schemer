; 6 Shadows

#lang racket

(require "ch01.rkt")
(require "ch04.rkt")

; (provide numbered?)
; (provide value)
; (provide value-prefix)
; (provide 1st-sub-exp)
; (provide 2nd-sub-exp)
; (provide operator)
; (provide value-prefix)
; (provide sero?)
; (provide edd1)
; (provide zub1)
; (provide edd)

; 假設 aexp 的 components 必為算術符號或數字
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
    
;(numbered? '(1 + 2))
;(numbered? '(1 + 2 * (3 ^ 5))) ; 誒誒誒誒?

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '^) (^ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      (else #f))))

;(value '(123 + 345))
;(value '(111 * 2))
;(value '(2 ^ 3))
;(value '((2 ^ 3) + 9))

; value-prefix
;(define value-prefix
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      ((eq? (car nexp) '+) (+ (value-prefix (car (cdr nexp))) (value-prefix (car (cdr (cdr nexp))))))
;      ((eq? (car nexp) '*) (* (value-prefix (car (cdr nexp))) (value-prefix (car (cdr (cdr nexp))))))
;      ((eq? (car nexp) '^) (^ (value-prefix (car (cdr nexp))) (value-prefix (car (cdr (cdr nexp))))))
;      (else #f))))

;(value-prefix '(^ 8 2))
;(value-prefix '(+ (* 3 6) (^ 8 2)))

; for prefix
(define 1st-sub-exp
  (lambda (axep) (car (cdr axep))))

(define 2nd-sub-exp
  (lambda (axep) (car (cdr (cdr axep)))))

(define operator
  (lambda (aexp) (car aexp)))

; Use help functions
(define value-prefix
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (+ (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '*) (* (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '^) (^ (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
      (else #f))))

;(value-prefix '(^ 8 2))
;(value-prefix '(+ (* 3 6) (^ 8 2)))

;Expression
; (): 0
; (()): 1
; (()()): 2
; (()()()): 3

(define sero?
  (lambda (n) (null? n)))

(define edd1
  (lambda (n) (cons '() n)))

(define zub1
  (lambda (n) (cdr n)))

; +
(define edd
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (edd n (zub1 m)))))))

;(edd '(()()()) '(()()))