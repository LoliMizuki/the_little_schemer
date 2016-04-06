; 6 Shadows

#lang racket

(require "Common.rkt")

; 假設 aexp 的 components 必為算術符號或數字
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
    
(numbered? '(1 + 2))
(numbered? '(1 + 2 * (3 ^ 5))) ; 誒誒誒誒?


  ; 受理數字, + X ^


; value ... 計算結果 ... 