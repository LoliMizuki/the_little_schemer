; 7. Friends and Relations

#lang racket

(require "ch05.rkt")

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

;(set? '(a b c))
;(set? '(a b a c))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cons (car lat) (makeset (multirember (car lat) lat)))))))

;(makeset '(apple peach pear peach plum apple lemon peach))

; 判斷 set1 是否為 set2 的 subset
(define subset?
  (lambda (set1 set2) 
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

;(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
;(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))