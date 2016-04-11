; 3. Cons the Magnificent

#lang racket

;(provide rember)
;(provide firsts)
;(provide insertR)
;(provide insertL)
;(provide subst)
;(provide subst2)
;(provide multirember)
;(provide multiinsertR)
;(provide multiinsertL)
;(provide multisubst)

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

; l is list of list
(define firsts
  (lambda (l)
    (cond
      ((null? l) l)
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
    
; (insertR 'e 'd '(a b c d f g h))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

; (insertL 'd 'e '(a b c e f g h))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old) (cons new (cdr lat)))
         (else (cons (car lat) (subst new old (cdr lat)))))))))

; (subst 'd 'e '(a b c e))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
         (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

; (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) a) (multirember a (cdr lat)))
         (else (cons (car lat) (multirember a (cdr lat)))))))))

; (multirember 'a '(a f g a t r a b a))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
         (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

; (multiinsertR 'z 'a '(a f g a t r a b a))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
         (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

; (multiinsertL 'z 'a '(a f g a t r a b a))

(define multisubst
(lambda (new old lat)
  (cond
    ((null? lat) '())
    (else
     (cond
       ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
       (else (cons (car lat) (multisubst new old (cdr lat)))))))))

;(multisubst 'z 'a '(a f g a t r a b a))