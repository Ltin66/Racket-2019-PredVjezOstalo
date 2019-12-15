;; my-modbeg.rkt
#lang racket
(require (for-syntax syntax/parse))

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out (my-#%module-begin #%module-begin)))

;; Helper for my-#%module-begin
;; (Yes, we're only checking the top-level right now...
;;  ...more on that later)
(define-for-syntax (find-add1s stx)
  (syntax-parse stx
    [(((~literal add1) args ...) . rst)
     (quasisyntax/loc stx
       ((displayln (format "You're calling add1 with ~a!" '(args ...)))
        (add1 args ...) #,@(find-add1s #'rst)))]
    [(((~literal sub1) args ...) . rst)
     (quasisyntax/loc stx
       ((displayln (format "You're calling sub1 with ~a!" '(args ...)))
        (sub1 args ...) #,@(find-add1s #'rst)))]
    [(other . rst) #`(other . #,(find-add1s #'rst))]
    [() stx]))

;; We'll name our #%module-begin something else
;; and rename it when exported
(define-syntax (my-#%module-begin stx)
  ;; Preprocess and delegate to the racket/base #%module-begin
  (syntax-parse stx
    [(_ . body) #`(#%module-begin #,@(find-add1s #'body))]))