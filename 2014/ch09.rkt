#lang racket

(require "ch01.rkt") ; for atom?
(require "ch07.rkt") ; for pair functions

; ch.9 ... and Again, and Again, and Again, ...
; argument 會在傳遞中被改變(有別于之前, 都是在原始的 argument 中的一部份走訪), 見 align

; looking, 尋找給定 a lat, 若走訪 lat 時, 遇到數字則尋找該位置是否等於 a ... 嗎????
; 但是又遇到數字的找法是? 循序 lat? 還是由最後的 index 開始?

(define pick
  (lambda (n lat)
    (cond
      ((null? lat) #f)
      ((= 1 n) (car lat))
      (else (pick (- n 1) (cdr lat))))))

; sorn: symbol or number, if number at n-th, else compare symbol and 'a'
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

; looking is a partial function ... it's may never ended (eg: tups, (3 2 1) <-- search 3 -> 1 -> 3 -> ...)
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

; 可能會永遠不停止的尋找喔

; 永遠的 ... 
(define eternity (lambda (x) (eternity x)))

; shift:
; The function shift takes a pair 
; whose first component is a pair and 
; builds a pair by shifting the second part of 
; the first component into the second component.
;
; 傳入一個 pair, 其第一個元素必為 pair,
; 裝第一個元素中的第二個組成移動到第二個元素中成為第一個組成.

(define shift
  (lambda (pair)
    (build 
     (first (first pair))
     (build 
      (second (first pair))
      (second pair)))))

;(shift (list (list 'a 'b) (list 'c 'd)))

; pora: pair or atom

; 對 pora 中的每一組都進行 shift?
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora))) ; 新的 align 呼叫, 使用了不同的 pora 參數
      (else (build (first pora) (align (second pora)))))))

; 可來計算 align 的 pora 中的參數個數, 但是沒有考慮權重, 所以是錯的噢.
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora)) (length* (second pora)))))))

; 考慮權重(first 都 *2)
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (weight* (first pora)) 2)
               (weight* (second pora)))))))

; weight* 推導
; 給定 ((a b) c)
; (2*(2*1 + 1) + 1) = 7
;
; 給定 (a (b c))
; (2 + (2*1 + 1)) = 5

; =====

; 與 align 同, 但是使用 revpair 取代 shift
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

;(shuffle (list 'a 'b))
;(shuffle (list 'a (list 'b 'c)))

; 奇偶歸一猜想(英語:Collatz conjecture),
; 又稱為3n+1猜想、冰雹猜想、角谷猜想、哈塞猜想、烏拉姆猜想或敘拉古猜想,
; 是指對於每一個正整數,
; 如果它是奇數,則對它乘3再加1,
; 如果它是偶數,則對它除以2,如此循環,最終都能夠得到1。
(define C 
  (lambda (n)
    (cond
      ((= 1 n) 1) 
      (else
       (cond
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))

; Ackermann function
; A(m, n)
; n + 1, if m = 0
; A(m-1, 1), if m > 0, n = 0
; A(m-1, A(m, n-1)), else
(define A
  (lambda (m n)
    (cond
      ((= m 0) (+ n 1))
      ((and (> m 0) (= n 0)) (A (sub1 m) 1))
      (else (A (sub1 m) (A m (sub1 n)))))))

; 嘗試做出一個函數能處理不特定數量的函數
; => 無可能 :DDDD

; ====

; define 問題

