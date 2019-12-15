;; defs.rkt
#lang racket
;; Because I like syntax-parse
(require (for-syntax syntax/parse))

(provide plus SEVEN def)

;; Adds the two given numbers together
(define/contract (plus a b)
  (number? number? . -> . number?)
  (+ a b))

;; Represents the median of the interval [6, 9)
(define SEVEN 7)

;; Defines a way to define without define
(define-syntax (def stx)
  (syntax-parse stx
    ; We don't really care what the contents are
    [(_ contents ...) (quasisyntax/loc stx (define contents ...))]))