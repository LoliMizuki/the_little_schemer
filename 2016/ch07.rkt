; 7. Friends and Relations

#lang racket

(require "ch01.rkt")
(require "ch05.rkt")

(provide a-pair?)
(provide revpair)

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

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

;(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

;(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))
;(intersect? '(stewed tomatoes and macaroni) '(cheese))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

;(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
;(intersect '(stewed tomatoes and macaroni) '(cheese))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

;(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))
    

;(intersectall '((1 2 3) (1 4 6) (1 8 9)))
;(intersectall '((a b c)
;                (c a d e)
;                (e f g h a b)))
;(intersectall '((6 pears and)
;                (3 peaches and 6 peppers)
;                (8 pear and 6 plums)
;                (and 6 prunes with some apples)))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p) (car p)))

(define second
  (lambda (p) (car (cdr p))))

(define third
  (lambda (p) (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2) (cons s1 (cons s2 '()))))

(define firsts
  (lambda (l-lst)
    (cond
      ((null? l-lst) '())
      (else (cons (first (car l-lst)) (firsts (cdr l-lst)))))))

(define fun?
  (lambda (rel) (set? (firsts rel))))

;(fun? '((4 3)
;        (4 2)
;        (7 6)
;        (6 2)
;        (3 4)))

;(fun? '((8 3)
;        (4 2)
;        (7 6)
;        (6 2)
;        (3 4)))

(define revpair
  (lambda (pair) (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

;(revrel '((8 a)
;          (pumpkin pie)
;          (got sick)))

(define seconds
  (lambda (l-lst)
    (cond
      ((null? l-lst) '())
      (else (cons (second (car l-lst)) (seconds (cdr l-lst)))))))

;(seconds '((a b) (1 2) (x y)))

(define fullfun?
  (lambda (rel) (set? (seconds rel))))

(define one-to-one?
  (lambda (fun) (fun? (revrel fun))))

;(one-to-one? '((chocolate chip) (doughy cookie)))