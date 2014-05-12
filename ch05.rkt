#lang racket

; ch05. *Oh My Gawd*; It's Full of Stars

(provide equal?)

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

;(rember* 'cup (list (list 'coffee) 'cup (list (list 'tea) 'cup) (list 'and (list 'hick)) 'cup))
;(rember* 'sauce (list (list (list 'tomato 'sauce)) (list (list 'bean) 'sauce) (list 'and (list (list 'flying)) 'sauce)))
;(miz-rember* 'cup (list (list 'coffee) 'cup (list (list 'tea) 'cup) (list 'and (list 'hick)) 'cup))
;(miz-rember* 'sauce (list (list (list 'tomato 'sauce))
;                          (list (list 'bean) 'sauce)
;                          (list 'and (list (list 'flying)) 'sauce)))

(define insertR*
  (lambda (new old l) 
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

; test
;(insertR* 'roast 'chuck (list (list 'how 'much (list 'wood)) 
;                              'could
;                              (list (list 'a (list 'wood) 'chuck)) (list (list (list 'chuck)))
;                              (list 'if (list 'a) (list (list 'wood 'chuck))) 'could 'chuck 'wood))

; count atom 'a' in list
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) 
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

; test
;(occur* 'banana (list (list 'banana)
;                      (list 'split (list (list (list (list 'banana 'ice))
;                                               (list 'cream (list 'banana))
;                                               'sherbet)) (list 'banana)
;                                                          (list 'bread)
;                                                          (list 'banana 'brand))))

; replce all atom 'a' in list
(define subst*
  (lambda (new old l)
    (cond 
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond 
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

; test
;(subst* 'orange 'banana (list (list 'banana)
;                              (list 'split (list (list (list (list 'banana 'ice))
;                                                       (list 'cream (list 'banana))
;                                                       'sherbet)) (list 'banana)
;                                                                  (list 'bread)
;                                                                  (list 'banana 'brand))))

; insert atom 'new' in left of atom 'old'
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (cons (car l) (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

; test 
;(insertL* 'orange 'banana (list (list 'banana)
;                                (list 'split (list (list (list (list 'banana 'ice))
;                                                         (list 'cream (list 'banana))
;                                                         'sherbet)) (list 'banana)
;                                                                    (list 'bread)
;                                                                    (list 'banana 'brand))))

; is atom 'a' in list?
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

; test
;(member* 'chips '((potato) (chips ((with) fish) (chips))))

(define leftmost
  (lambda (l) 
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; test
;(leftmost (list (list 'potato) (list 'chips (list (list 'with) 'fish) (list 'chips))))
;(leftmost (list (list (list 'hot) (list 'tuna (list 'and))) 'cheese))

; is equal of s-express 's1' 's2'
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; is s-expression 's1' 's2' equal?
(define miz-eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ; use equal? function
      (else (and (equal? (car l1) (car l2)) (miz-eqlist? (cdr l1) (cdr l2)))))))
; if has't equal? function
;      ((and (list? (car l1)) (list? (car l2))) (miz-eqlist? (car l1) (car l2)))
;      ((and (atom? (car l1)) (atom? (car l2)) (eqan? (car l1) (car l2))) (miz-eqlist? (cdr l1) (cdr l2)))
;      (else #f))))

; not simplify :D, miz-eqlist? is simplified
(define eqlist? 
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t) ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (atom? (car l1)) (null? l2)) #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))) ((atom? (car l1)) #f)
                                          ((null? l2) #f)
                                          ((atom? (car l2)) #f)
                                          (else
                                           (and (eqlist? (car l1) (car l2))
                                                (eqlist? (cdr l1) (cdr l2)))))))

; test
;(miz-eqlist? '(strawberry ice cream) '(strawberry ice cream))
;(miz-eqlist? '(strawberry ice cream) '(strawberry cream ice))
;(miz-eqlist? '(banana ((split))) '((banana) (split)))      
;(miz-eqlist? '(beef ((sausage)) (and (soda)))
;             '(beef ((salami)) (and (soda))))
;(miz-eqlist? '(beef ((sausage)) (and (soda))) 
;             '(beef ((sausage)) (and (soda))))

;(eqlist? '(strawberry ice cream) '(strawberry ice cream))
;(eqlist? '(strawberry ice cream) '(strawberry cream ice))
;(eqlist? '(banana ((split))) '((banana) (split)))      
;(eqlist? '(beef ((sausage)) (and (soda)))
;         '(beef ((salami)) (and (soda))))
;(eqlist? '(beef ((sausage)) (and (soda))) 
;         '(beef ((sausage)) (and (soda))))

; 有人叫我簡化他 :D
(define rember-s
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember-s s (cdr l)))))))

; test
; (rember-s 'chips (list (list 'potato) (list 'chips (list (list 'with) 'fish) (list 'chips))))

