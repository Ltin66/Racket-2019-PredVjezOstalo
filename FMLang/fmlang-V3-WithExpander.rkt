#lang racket
;#lang br/quicklang

(define (read-syntax path port)
  (define be-syn '(module lucy br))
  (define src-lines (port->lines port))
  (define (eval cntr lst)
    (cond
      ([eq? cntr 10] lst)
      (else (eval (+ 1 cntr) (append lst '((display 'tin)) '((display "\n")))))))
  (datum->syntax #f (eval -20 be-syn)))
     



  
 ; (define src-lines (port->lines port))
  ;read everything from port
  ;src-lines contains the port content (file content)
  ;(datum->syntax #f '(module lucy br
   ;                    42)))

;(module lucy br 42)
;Means “a module named lucy, using the expander from
;the br language, that eval­u­ates the expres­sion 42.”

(provide read-syntax)
 
