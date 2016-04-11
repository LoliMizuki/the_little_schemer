; 5 *Oh My Gawd*: It's Full of Stars

#lang racket

(require "ch01.rkt")
(require "ch04.rkt")

(provide member?)
(provide rember)
(provide rember*)
(provide multirember)
;(provide insertR)
;(provide insertL)
;(provide occur)
;(provide subst)
;(provide leftmost)
;(provide eqlist)
;(provide equal)

; 後面有重新定義
;(define rember*
;  (lambda (a l)
;    (cond
;      ((null? l) '())
;      (else
;       (cond
;         ;((and (atom? (car l)) (eq? a (car l))) (rember* a (cdr l)))
;         ((atom? (car l))
;          (cond
;            ((eq? a (car l)) (rember* a (cdr l)))
;            (else (cons (car l) (rember* a (cdr l))))))
;         ; ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l)))) 以下的 else 處理了包含 (car l) 是 list 的狀態
;         (else (cons (rember* a (car l)) (rember* a (cdr l)))))))))


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

(define leftmost*
   (lambda (l)
     (cond
       ((null? l) #f) ; 多檢查一個, for No answer
       ((atom? (car l)) (car l))
       (else (leftmost* (car l))))))

;(define eqlist?
;  (lambda (l1 l2)
;    (cond
;      ((and (null? l1) (null? l2)) #t)
;      ((or (null? l2) (null? l1)) #f)
;      ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;      ((or (atom? (car l1)) (atom? (car l2))) #f)
;      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

;(eqlist? '(banan ((split))) '((banana) (split)))

; 比較兩個 S-Expression
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; 用 equal? 重寫 eqlist?

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null l2)) #t)
      ((or (null? l1) (null l2)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

; 用 equal? 重新定義 rember
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))

; 用 equal? 重新定義 rember*

; 重新定義 ch02 的 member?, eqaul? 取代 eq?
(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      (else (or (equal? a (car l)) (member? a (cdr l)))))))

;(member? 'a '(a b c 1 2 3))
;(member? 2 '(a b c 1 2 3))
;(member? 'z '(a b c 1 2 3))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      (else
       (cond
         ((atom? (car l))
          (cond
            ((equal? a (car l)) (rember* a (cdr l)))
            (else (cons (car l) (rember* a (cdr l))))))
         (else (cons (rember* a (car l)) (rember* a (cdr l)))))))))

;(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
;(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
;(rember* 1 '(1 1 1 1 2 3 4))

; 用 eqaul? 重新定義 multirember
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((equal? (car lat) a) (multirember a (cdr lat)))
         (else (cons (car lat) (multirember a (cdr lat)))))))))