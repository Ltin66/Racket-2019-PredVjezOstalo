#lang sicp
; map , filter, length
; reverse, count predicate?
; accumulate, member, unique
; flatten, flatmap, zip


(define (map proc lst)
  (cond
    ([null? lst] '())
    (else (append (list (proc (car lst))) (map proc (cdr lst))))))


(define (filter pred? lst)
  (cond
    ([null? lst] '())
    ([pred? (car lst)] (append (list (car lst)) (filter pred? (cdr lst))))
    (else (filter pred? (cdr lst)))))

(define (length lst)
  (cond
    ([null? lst] 0)
    (else (+ 1 (length (cdr lst))))))


(define (reverse lst)
  (cond
    ([null? lst] '())
    (else (append (reverse (cdr lst)) (list (car lst))))))
  

(define (count-predicate pred? lst)
  (cond
    ([null? lst] 0)
    ([pred? (car lst)] (+ 1 (count-predicate pred? (cdr lst))))
    (else (count-predicate pred? (cdr lst)))))

(define (count-predicateV2 pred? lst)
  (length (filter pred? lst)))


(define (accumulate proc init lst)
  (define (accumulate-rek proc lst)
    (cond
      ([null? lst] init)
      (else (proc (car lst) (accumulate-rek proc (cdr lst))))))
  (proc (car lst) (accumulate-rek proc (cdr lst))))
      

(define (member elem lst)
  (cond
    ([null? lst] #f)
    ([eq? elem (car lst)] #t)
    (else (member elem (cdr lst)))))

(define (unique lst)
  (cond
    ([null? (cdr lst)] #t)
    ([member (car lst) (cdr lst)] #f)
    (else (unique (cdr lst)))))


(define (flatten lst)
  (cond
    ([null? lst] '())
    ([pair? (car lst)] (append (flatten (car lst)) (flatten (cdr lst))))
    (else (append (list (car lst)) (flatten (cdr lst))))))


(define (flatmap proc lst)
  (accumulate append '() (map proc lst)))


(flatmap (lambda (n) (if (odd? n) (list (* n n)) '())) '(1 2 3 4 5))

(flatmap (lambda (n) (if (odd? (car n)) (list (* 2 (car (cdr n)))) '()))
         '( (1 4) (2 8) (3 7) (9 2)))


(flatmap  (lambda (x) (map (lambda (y) (list y x)) '(1 2 3)))
          '(4 5 6))

(define (cartesian-product lst1 lst2)
  (flatmap  (lambda (x) (map (lambda (y) (list y x)) lst1))
          lst2))
  
(define (double-num-list n)
  (list n n))

;(define (

(flatmap double-num-list '(1 2 3 4))








































