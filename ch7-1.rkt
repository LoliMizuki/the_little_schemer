#lang racket

(require "01.rkt")

; is lat a set?
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat)))))) 

;(set? (list 'a 'b 'c))
;(set? (list 'a 'b 'c 'a))
;(set? (list 'apple 3 'pear 4 9 'apple 3 4))
;(set? (list 1 2 3))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

; use multirember
(define makeset-ex
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons 
             (car lat)
             (makeset-ex (multirember (car lat) (cdr lat))))))))

; test
;(makeset-ex (list 'a 'b 'c 'd 'a))
;(makeset-ex (list 'apple 3 'pear 4 9 'apple 3 4))

; is set1 subset of set2?
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

; test
;(subset?
; (list 5 'chicken 'wings)
; (list 5 'hamburgers 2 'pieces 'fried 'chicken 'and 'light 'duckling 'wings))

;(subset? 
; (list 4 'pounds 'of 'horseradish)
; (list 'four 'pounds 'chicken 'and '5 'ounces 'horseradish))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

; test
;(eqset? (list 'a 'b 'c 'd 'a) (list 'a 'b 'c 'd 'a))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

; test
;(intersect? (list 'stewed 'tomatoes 'and 'macaroni)
;            (list 'macaroni 'and 'cheese))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

;(intersect (list 'stewed 'tomatoes 'and 'macaroni)
;           (list 'macaroni 'and 'cheese))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

;(union (list 'stewed 'tomatoes 'and 'macaroni)
;       (list 'macaroni 'and 'cheese))


(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

;(intersectall (list (list 6 'pears 'and)
;                    (list 3 'peaches 'and 6 'peppers)
;                    (list 8 'pears 'and 6 'plums)
;                    (list 'and 6 'prunes 'with 'some 'apples)))

; pair: has two s-expression
; ('g '(k)) is pair

(define a-pair?
  (lambda (x) 
    (cond 
      ((atom? x) #f)
      ((null? (car x)) #f)
      ((null? (cdr x)) #f)
      ((null? (car (car x))) #f) ; book is error here :D
      (else #t))))

;(a-pair? '('a 'b))

(define first (lambda (p) (car p)))
(define second (lambda (p) (car (cdr p))))
(define third (lambda (p) (car (cdr (cdr p)))))

; build pair
(define build
  (lambda (x1 x2)
    (cons x1 (cons x2 (quote ())))))

; rel: a set list of pair **注意, 要是 set 喔**

; fun: 一組 rel, 且每個 pair 的第一個元素集合是一個 set
; ex:
;    ((4 3) (4 2) (7 6) (6 2) (3 4)) => firsts 為 (4 4 7 6 3) 不是集合
;    ((8 3) (4 2) (7 6) (6 2) (3 4)) => firsts 為 (8 4 7 6 3) 是集合
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))
;(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

(define revpair (lambda (p) (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

;(revrel '((8 'a) ('pumpkin 'pie) ('got 'sick)))

; define seconds as firsts
(define seconds
  (lambda (l)
    (cond 
      ((null? l) (quote ()))
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

;(seconds (list (list 'a 'b) (list 'c 'd) (list 3 4)))

; fullfun: a list of pairs, 每個第二個元素的集合是一個 set
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? 
 (list (list 'grape 'raisin) (list 'plum 'prune) (list 'stewed 'grape)))

(fullfun? 
 (list (list 'grape 'raisin) (list 'plum 'grape) (list 'stewed 'grape)))
