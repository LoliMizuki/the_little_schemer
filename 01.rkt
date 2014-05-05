#lang racket

(provide atom?)

(define atom? (lambda (a)  (not (list? a))))

(define lat? 
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


(define miz-lat? 
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((not (atom? (car l))) #f)
      (else (lat? (cdr l))))))

(define member? 
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(define miz-member? 
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat) 
    (cond 
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

; atom in lat will all remove
; ex: (rember a '(a b a c)) > '(b c)
; 與 multirember 相同
(define rember-all-of
  (lambda (a lat) 
    (cond 
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (rember-all-of a (cdr lat)))
      (else (cons (car lat) (rember-all-of a (cdr lat)))))))

(define firsts 
  (lambda (l) 
    (cond ((null? l) (quote ()))
          (else (cons (car (car l))
                      (firsts (cdr l)))))))

(define ll1 (list 
             (list 'a 'b)
             (list 'c 'd)
             (list 'e 'f)))

(define interR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (interR new old (cdr lat)))))))

(define interL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (interL new old (cdr lat)))))))

(define subst 
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2 
  (lambda (new o1 o2 lat) 
    (cond 
      ((null? lat)
       (quote ()))
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) 
       (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multiinsertR 
  (lambda (new old lat) 
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) 
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat) 
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) 
       (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

; "-- test code --"

;(define l '(a b c d e c f))
;(rember 'c l)
;(rember-all-of 'c l)
;(interR 'neko 'mizuki (list 'i 'want 'a 'mizuki))
;(interL 'neko 'mizuki (list 'i 'want 'a 'mizuki))
;(subst 'neko 'mizuki (list 'i 'want 'a 'mizuki))
;(subst2 'neko 'mizuki 'yuyuko (list 'eat 'yuyuko 'i 'want 'a 'mizuki))
;(multiinsertR 'bitch 'mizuki (list 'a ' little 'mizuki 'want 'a 'mizuki))
;(multiinsertL 'bitch 'mizuki (list 'a ' little 'mizuki 'want 'a 'mizuki))
;(multiinsertL 'fried 'fish (list 'chips 'and 'fish 'or 'fish 'and 'fried))
;(multisubst 'a 'b (list 'c 'a 'b 'g 'b))