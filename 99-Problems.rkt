#lang sicp


;http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html


;P01 Find the last box of a list.

(define (my-last lst)
  (cond
    ([null? (cdr lst)] (car lst))
    (else (my-last (cdr lst)))))
                       
;P02 (*) Find the last but one box of a list.

(define (my-but-last lst)
  (cond
    ([null? (cdr (cdr lst))] lst)
    (else (my-but-last (cdr lst)))))
  

;P03 (*) Find the K'th element of a list.
;ACTHUNG starts from 0
(define (element-at lst k)
  (cond
    ([null? lst] '())
    ([eq? k 1] (car lst))
    (else (element-at (cdr lst) (- k 1)))))


;P04 (*) Find the number of elements of a list.

(define (count-list lst)
  (cond
    ([null? lst] 0)
    (else (+ 1 (count-list (cdr lst))))))


;P05 (*) Reverse a list.

(define (reverse lst)
  (cond
    ([null? lst] '())
    (else (append (reverse (cdr lst)) (list (car lst))))))


;P06 (*) Find out whether a list is a palindrome. 

(define (palindrome? lst)
  (define (pal? lst rev-lst)
    (cond
      ([null? lst] #t)
      ([eq? (car lst) (car rev-lst)] (pal? (cdr lst) (cdr rev-lst)))
      (else #f)))
  (pal? lst (reverse lst)))

;P07 (**) Flatten a nested list structure. 

(define (flatten lst)
  (cond
    ([null? lst] '())
    ([pair? (car lst)] (append (flatten (car lst)) (flatten (cdr lst)))) 
    (else (append (list (car lst)) (flatten (cdr lst))))))



;P08 (**) Eliminate consecutive duplicates of list elements. 
;If a list contains repeated elements they should be
;replaced with a single copy of the element. The order of the elements should not be changed.

(define (compress lst)
  (cond
    ([null? (cdr lst)] (list (car lst)))
    ([eq? (car lst) (car (cdr lst))] (compress (cdr lst)))
    (else (append (list (car lst)) (compress (cdr lst))))))


;P09 (**) Pack consecutive duplicates of list elements into sublists.


(define (pack lst)
  (define (find-first-different lst)
    (cond
      ([null? (cdr lst)] '())
      ([eq? (car lst) (car (cdr lst))] (find-first-different (cdr lst)))
      (else (cdr lst))))
  (define (pack-until-different lst)
    (cond
      ([null? (cdr lst)] (list (car lst)))
      ([eq? (car lst) (car (cdr lst))]
       (append (list (car lst)) (pack-until-different (cdr lst))))
      (else (list (car lst)))))
  (cond
    ([null? lst] '())
    (else (append '() (cons (pack-until-different lst)
                  (pack (find-first-different lst)))))))
      
     
;P10 (*) Run-length encoding of a list.
;P11 , za P10 samo se izbrise jedan dio

(define (encode lst)
  (define (enc-rek lst-tmp)
    (cond
      ([null? lst-tmp] '())
      ([eq? (count-list (car lst-tmp)) 1]
       (append (list (car (car lst-tmp))) (enc-rek (cdr lst-tmp))))
      (else (append
             (list (append
                    (list (count-list (car lst-tmp)))
                    (list (car (car lst-tmp)))))
             (enc-rek (cdr lst-tmp))))))
  (enc-rek (pack lst)))
       
     
;P12 (**) Decode a run-length encoded list. 

(define (decoder lst)
  (define (make-list-from-elem elem num)
    (cond
      ([eq? num 0] '())
      (else (append (list elem) (make-list-from-elem elem (- num 1))))))
  (cond
    ([null? lst] '())
    ([pair? (car lst)]
     (append
      (make-list-from-elem (car (cdr (car lst))) (car (car lst)) )
      (decoder (cdr lst))))
    (else (append (list (car lst)) (decoder (cdr lst))))))


;P13 (**) Run-length encoding of a list (direct solution). 
; da


;P14 (*) Duplicate the elements of a list.

(define (duplicate lst)
  (cond
    ([null? lst] '())
    (else (append (list (car lst)) (list (car lst)) (duplicate (cdr lst))))))

;P15 (**) Replicate the elements of a list a given number of times. 

(define (replicate lst number-of-times)
  (define (make-list-from-elem elem num)
    (cond
      ([eq? num 0] '())
      (else (append (list elem) (make-list-from-elem elem (- num 1))))))
  (cond
    ([null? lst] '())
    (else (append (make-list-from-elem (car lst) number-of-times)
                  (replicate (cdr lst) number-of-times)))))

;tjt, bravo ja






























































































































