; 5 *Oh My Gawd*: It's Full of Stars

#lang racket

(require "Common.rkt")

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      (else
       (cond
         ((and (atom? (car l)) (eq? a (car l))) (rember* a (cdr l)))
         ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
         (else (cons (car l) (rember* a (cdr l)))))))))


;(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
;(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))