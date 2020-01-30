;#lang br/quicklang
#lang racket
 

(define (read-syntax path port)
  (define port-str (port->string port))
  
  ;(define ni-st (open-input-string port-str))
  ;(define (get-char-from-port) (read-char ni-st))
  ;(define src-lines (port->lines port))
  ;read everything from port
  ;src-lines contains the port content (file content)
  (datum->syntax #f '(module lucy racket
                       (display 10))))

;(module lucy br 42)
;Means “a module named lucy, using the expander from
;the br language, that eval­u­ates the expres­sion 42.”

(provide read-syntax)

; '(module lucy br 42)
; same as :
;  (quote (module lucy br 42))


