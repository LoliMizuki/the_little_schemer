; 9. ... and Again, and Again, and Again, ...

#lang racket

(require "ch01.rkt")
(require "ch04.rkt")
(require "ch07.rkt")

(provide build)
(provide first)
(provide second)

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

;(length* (align '((a b) c)))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (weight* (first pora)) 2) (weight* (second pora)))))))

;(weight* '((a b) c))
;(weight* '(a (b c)))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

;(shuffle '((a b) c))
;(shuffle '(a b))
;(shuffle '((a b) (c d)))

; Collatz conjecture (https://zh.wikipedia.org/wiki/%E8%80%83%E6%8B%89%E5%85%B9%E7%8C%9C%E6%83%B3)
; 對所有自然數, 如果它是奇數, 則對它乘3再加1, 如果它是偶數, 則對它除以2, 如此循環, 最終都能夠得到1.
; by Lothar Collatz (https://en.wikipedia.org/wiki/Lothar_Collatz)
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (/ n 2)))
      (else (C (+ (* n 3) 1))))))

; Ackermann function
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m))))))) 

;(A 1 0)
;(A 1 1)
;(A 2 2)

; 使用 lambda 匿名的情形下定義`遞迴`

;((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 ((mk-length mk-length) (cdr l))))))))

; 驗證
;(((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 ((mk-length mk-length) (cdr l)))))))) '(apple))

;(((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 ((mk-length mk-length) (cdr l)))))))) '(apple banner))


; 小測試
;((lambda (mkc) (mkc mkc))
; (lambda (mkc)
;   (lambda (x y)
;     (cons x (cons y '())))))

;(((lambda (mkc) (mkc mkc))
; (lambda (mkc)
;   (lambda (x y)
;     (cons x (cons y '()))))) 'a 'b)

; Y-Conbination
(define Y
  (lambda (le)
    (lambda (f) (f f))
    (lambda (f)
      (le (lambda (x) ((f f) x))))))