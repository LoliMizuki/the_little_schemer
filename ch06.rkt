#lang racket

; ch06. Shadows

(require "01.rkt")

; is arithmetic expression ... ???
(define numbered?
  (lambda (aexp)
    (cond 
      ((atom? aexp) (number? aexp))
      (else (and (car aexp) (numbered? (car (cdr (cdr aexp)))))))))
;((eq? (car (cdr aexp)) (quote +)) (and (car aexp) (numbered? (car (cdr (cdr aexp))))))
;((eq? (car (cdr aexp)) (quote *)) (and (car aexp) (numbered? (car (cdr (cdr aexp))))))
;((eq? (car (cdr aexp)) (quote ^)) (and (car aexp) (numbered? (car (cdr (cdr aexp)))))))))

; test
;(numbered? 3)
;(numbered? '(1 + 3))

; 計算中序式 (1 + 2)
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp)
    (cond
      ((and (atom? nexp) (number? nexp)) nexp)
      ((eq? (operator nexp) (quote +)) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote *)) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote ^)) (expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

; test
;(value '(1 + 2))
;(value '(3 * 2))
;(value '(2 ^ 4))

; 計算前序式 (+ 1 2)
(define 1st-sub-exp-pre
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp-pre
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator-pre
  (lambda (aexp)
    (car aexp)))

(define value-pre
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator-pre nexp) (quote +)) 
       (+ (value-pre (1st-sub-exp-pre nexp)) (value-pre (2nd-sub-exp-pre nexp))))
      ((eq? (operator-pre nexp) (quote *))
       (* (value-pre (1st-sub-exp-pre nexp)) (value-pre (2nd-sub-exp-pre nexp))))
      ((eq? (operator-pre nexp) (quote ^))
       (expt (value-pre (1st-sub-exp-pre nexp)) (value-pre (2nd-sub-exp-pre nexp)))))))

; test
;(value-pre '(+ 7 8))
;(value-pre '(* 7 8))
;(value-pre '(^ 4 3))

; representation for below
; () is zero, ( () ) is one, ( () () ) is two ...

; () is zero
(define pzero?
  (lambda (n)
    (null? n)))

(define padd1
  (lambda (n)
    (cons (quote ()) n))) ;(cons '() n)))

(define psub1
  (lambda (n)
    (cdr n)))

(define p+
  (lambda (n m)
    (cond
      ((pzero? m) n)
      (else (padd1 (p+ n (psub1 m)))))))

(p+ '(() ()) '(() () ()))