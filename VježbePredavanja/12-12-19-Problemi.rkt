#lang racket


;Probem 1
;define a function sorted that takes a list and checks wether its sorted



(define (sorted? lst)
  (define (sort-up? lst)
    (cond
      ([null? lst] #t)
      ([null?(cdr lst)] #t) 
      ([>= (car lst) (car (cdr lst))] (sort-up? (cdr lst)))
      (else #f)))
  
  (define (sort-dwn? lst)
    (cond
      ([null? lst] #t)
      ([null?(cdr lst)] #t) 
      ([<= (car lst) (car (cdr lst))] (sort-dwn? (cdr lst)))
      (else #f)))

  (or (sort-up? lst) (sort-dwn? lst)))
       
       
 (sorted? '(1 1 2 3 4 88 88.1))

;Sub problem

(define (nonincreasing? lst)
  (define (inc? lst)
    (cond
      ([null? lst] #t)
      ([null?(cdr lst)] #t) 
      ([>= (car lst) (car (cdr lst))] (inc? (cdr lst)))
      (else #f)))
  (inc? lst))



;SUBSUB problem
;devise a fun taht takes a lst and returns a lst of consecutive pairs
;pair-up '(1 2 3 ) -> '( (1 2) (2 3))




(define (pair-step x y)
  (cond
    ([null? y] (cons x '()))
    ([null? (cdr y)] (cons x (list (cons x (car y)))))
    (else (cons x (cons (cons x (car y)) (cdr y))))))


;Accumulate,FoldR,Reduce,Compress,Inject
(define (accumulate proc init lst)
  (cond
    ([null? lst] init)
    (else (proc (car lst) (accumulate proc init (cdr lst))))))          



;MAP
;apply proc to every elem
(define (map proc lst)
  (cond
    ((null? lst) '())
    (else (cons (proc (car lst)) (map proc (cdr lst))))))




;(define (pair-up lst) (cdr (accumulate  pair-step '() lst)))



;(map (lambda (pair) (< (car pair) (cdr pair))) (pair-up '()))

;(define (all lst) ( accumulate (lambda (x y) (and x y)) #t lst;))


;Problem 2
; (dropwhile predicate? lst)
; returns tail of lst where the first elem no longer satisfies pred?



(define (drop-while pred? lst)
  (cond
    ([null? lst] '())
    ([pred? (car lst)] (drop-while pred? (cdr lst)))
    (else lst)))



;Problem 3 Zip
; zip operator that takes two lists and gives  a list of pairs of
; those elements. the len of out should be the len of shorter list

; (zip '(1 2 3 4) '(a b c d)) -> '((1 . a) (2 . b) (3 . c) (4 . d))

(define (zip lst1 lst2)
  (define (zip-iter lst1 lst2 lst-rez)
    (cond
      ([null? lst1] lst-rez)
      ([null? lst2] lst-rez)
      (else (zip-iter (cdr lst1) (cdr lst2)
                      (cons (cons (car lst1) (car lst2))
                            lst-rez)))))
  (zip-iter lst1 lst2 '()))



(define (zip-rek l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))




;Problem 4 , define a function reverse that reverses a list

;Problem 5 , define th efunction count-consecutive that  given a list
; produces a list of pairs such that the first elem
;of the pair is an elem of the list and the secon elem is the number
;of consecutive repetitions



;(count-consecutive '(a a a a b b c c c d))
;-> '( (a . 4) ( b . 2) ( c . 3) (d . 1)) 



;Problem 6
;given a list of pairs (a . b) where b pair is nonegative num
; produce a list of lists of b repertitions of elem a.
;group-expand

;(group-expand '((a . 4) (b . 3))) -> ((a a a a) (b b b))












































