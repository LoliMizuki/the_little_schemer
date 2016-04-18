; 9. ... and Again, and Again, and Again, ...

#lang racket

(require "ch01.rkt")
(require "ch07.rkt")

(define pick
  (lambda (n lat)
    (cond
      ((null? lat) #f)
      ((= n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

; sorn => symbol or number
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((null? lat) #f)
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define looking
  (lambda (a lat) (keep-looking a (pick 1 lat) lat)))

;(looking 'caviar '(6 2 4 caviar 5 7 3))
;(looking 'caviar '(6 2 4 aaaa 5 7 3))

; 永遠不結束
(define eternity (lambda (x) (eternity x)))

; 傳入一個 pair (f, s)
; 該 pair 的第一個 element f 是 pair
; 將 f 中第二個 element 併到 2nd pair s 中
(define first (lambda (pair) (car pair)))
(define second (lambda (pair) (car (cdr pair))))    
(define build (lambda (a b) (cons a (cons b '()))))
(define shift
  (lambda (pair)
    (build (first (first pair)) (build (second (first pair)) (second pair)))))

;(shift '((a b) c))
;(shift '((a b) (c d)))

; 對所有 pair 做 shift 處理
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

; 計算 align 的輸出用
; atom 數?
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora)) (length* (second pora)))))))

(length* (align '((a b) c)))


