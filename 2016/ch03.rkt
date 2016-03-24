; 3. Cons the Magnificent

#lang racket

(define remeber
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (remeber a (cdr lat)))
      (else (cons (car lat) (remeber a (cdr lat)))))))

