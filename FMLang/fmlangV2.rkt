#lang racket
;#lang br/quicklang

(define (read-syntax path port)
  (define be-syn '())
  (define src-lines (port->lines port))
  (define (eval-a cntr lst)
    (cond
      ([eq? cntr 10] (list lst))
      (else (eval-a (+ 1 cntr) (append lst '((display 'test)) '((display "\n")))))))
  (define x 2323)
  (datum->syntax #f (append '(module lucy racket) (eval-a -10 be-syn))))
     



  
 ; (define src-lines (port->lines port))
  ;read everything from port
  ;src-lines contains the port content (file content)
  ;(datum->syntax #f '(module lucy br
   ;                    42)))

;(module lucy br 42)
;Means “a module named lucy, using the expander from
;the br language, that eval­u­ates the expres­sion 42.”

(provide read-syntax)
 
