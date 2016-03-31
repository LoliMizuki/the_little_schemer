; 5 *Oh My Gawd*: It's Full of Stars

#lang racket

(require "Common.rkt")

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      (else
       (cond
         ;((and (atom? (car l)) (eq? a (car l))) (rember* a (cdr l)))
         ((atom? (car l))
          (cond
            ((eq? a (car l)) (rember* a (cdr l)))
            (else (cons (car l) (rember* a (cdr l))))))
         ; ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l)))) 以下的 else 處理了包含 (car l) 是 list 的狀態
         (else (cons (rember* a (car l)) (rember* a (cdr l))))))))) ; 


;(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
;(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
       (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

;(insertR* 'roast 'chuck '((how much (wood))
;                          colud
;                          ((a (wood) chuck))
;                          (((chuck)))
;                          (if (a) ((wood chuck)))
;                          could chuck wood))
; Ans:
; '((how much (wood))
;   colud
;   ((a (wood) chuck roast))
;   (((chuck roast)))
;   (if (a) ((wood chuck roast)))
;   could
;   chuck
;   roast
;   wood)

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

;(occur* 'banana '((banana)
;                 (split ((((banana ice)))
;                         (cream (banana))
;                         sherbet))
;                 (banana)
;                 (bred)
;                 (banana brandy)))
; Ans: 5

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
       (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

;(subst* 'orange 'banana '((banana)
;                          (split ((((banana ice)))
;                                  (cream (banana))
;                                  sherbet))
;                          (banana)
;                          (bred)
;                          (banana brandy)))
; Ans:
;'((orange)
;  (split ((((orange ice))) (cream (orange)) sherbet))
;  (orange)
;  (bred)
;  (orange brandy))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
       (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

;(insertL* 'pecker 'chuck '((how much (wood))
;                           colud
;                           ((a (wood) chuck))
;                           (((chuck)))
;                           (if (a) ((wood chuck)))
;                           could chuck wood))
; Ans:
;'((how much (wood))
;  colud
;  ((a (wood) pecker chuck))
;  (((pecker chuck)))
;  (if (a) ((wood pecker chuck)))
;  could
;  pecker
;  chuck
;  wood)

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

;(member* 'chips '((potato) (chips ((with) fish) (chips))))
;(member* 'kulala '((potato) (chips ((with) fish) (chips))))
