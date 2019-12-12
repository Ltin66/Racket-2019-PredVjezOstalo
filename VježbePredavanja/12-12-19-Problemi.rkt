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
  







