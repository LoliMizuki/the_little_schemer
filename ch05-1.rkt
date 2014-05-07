#lang racket

(require "01.rkt")
(require "02.rkt")
(require "03.rkt")

(define miz-rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
      ((eq? a (car l)) (rember* a (cdr l)))
      (else (cons (car l) (rember* a (cdr l)))))))

(define rember*
  (lambda (a l)
    (cond 
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((eq? a (car l)) (rember* a (cdr l)))
                         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

; (rember* 'cup (list (list 'coffee) 'cup (list (list 'tea) 'cup) (list 'and (list 'hick)) 'cup))
; (miz-rember* 'cup (list (list 'coffee) 'cup (list (list 'tea) 'cup) (list 'and (list 'hick)) 'cup))

; (rember* 'sauce (list (list (list 'tomato 'sauce)) (list (list 'bean) 'sauce) (list 'and (list (list 'flying)) 'sauce)))
; (miz-rember* 'sauce (list (list (list 'tomato 'sauce)) (list (list 'bean) 'sauce) (list 'and (list (list 'flying)) 'sauce)))

(define insertR*
  (lambda (new old l) 
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) 
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l)))))
       (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))))
  
  ; insertR* test data
  (list (list 'how 'much (list 'wood)) 
        'could
        (list (list 'a (list 'wood) 'chuck)) (list (list (list 'chuck)))
        (list 'if (list 'a) (list (list 'wood 'chuck))) 'could 'chuck 'wood)