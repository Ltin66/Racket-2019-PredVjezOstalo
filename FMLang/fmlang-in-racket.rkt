#lang racket


(define ns (make-base-namespace))
(define-namespace-anchor a)
(define fml-lst '((display 'red1)(+ 2 3)(set! rowCnt (+ 1 rowCnt) )(display rowCnt)))
(define rowCnt 2)
(define nsa (namespace-anchor->namespace a))

(define (list-eval lst)
  (cond
    ([null? lst] #t)
    (else (eval (car lst) nsa) (list-eval (cdr lst)))))



(eval '(display 'dwadwa) ns)
