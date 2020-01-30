#lang br/quicklang

(define (read-syntax path port)
  (define (get-char-from-port) (read-char port))
  (define (form-syntax-from-port portA syn-lst syn-tmp)
    (let (()
    (cond
      ([char-whitespace? (peek-char port)] (
       (port-commit-peeked
    (let ((comm (append (list 







  
 ; (define src-lines (port->lines port))
  ;read everything from port
  ;src-lines contains the port content (file content)
  ;(datum->syntax #f '(module lucy br
   ;                    42)))

;(module lucy br 42)
;Means “a module named lucy, using the expander from
;the br language, that eval­u­ates the expres­sion 42.”

(provide read-syntax)

; '(module lucy br 42)
; same as :
;  (quote (module lucy br 42))


