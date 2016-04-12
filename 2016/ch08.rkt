; 8. Lambda the Ultimate

#lang Racket

(require "ch05.rkt")

;(define rember-f
;  (lambda (test? a l)
;    (cond
;      ((null? l) '())
;      ((test? a (car l)) (rember-f test? a (cdr l)))
;      (else (cons (car l) (rember-f test? a (cdr l)))))))

;(rember-f = 5 '(6 2 5 3))
;(rember-f eq? 'jelly '(jelly beans are good))
;(rember-f equal? '(pop corn) '(jelly beans are (pop corn) good))

; c: Currying
(define eq?-c
  (lambda (a) (lambda (x) (eq? a x))))

(define eq?-salad (eq?-c 'salad))

;(eq?-salad 'salad)
;(eq?-salad 'tuna)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) ((rember-f test?) a (cdr l)))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))
;(rember-eq? 'jelly '(jelly beans are good))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons new (cons old ((insertL-f test?) new old (cdr l)))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons old (cons new ((insertR-f test?) new old (cdr l)))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l) (cons new (cons old l))))

(define seqR
  (lambda (new old l) (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old ((insert-g seq) new old (cdr l))))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

;(define insertL (insert-g seqL))
(define insertL (insert-g (lambda (new old l) (cons new (cons old l)))))
;(insertL 'z 'a '(c v a d g))

;(define insertR (insert-g seqR))
(define insertR (insert-g (lambda (new old l) (cons old (cons new l)))))
;(insertR 'z 'a '(c v a d g))

(define subst (insert-g (lambda (new old l) (cons new l))))

(define seqrem (lambda (new old l) l))
(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l))) ; #f 為原本 (insert-g new old l) 中的 new

;(rember 'sausage '(pizza with sausage and bacon))