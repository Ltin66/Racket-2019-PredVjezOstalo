#lang sicp

(define (stream-cons a b) (cons a (delay b)))

(define (stream-cdr s) (force (cdr s)))
(define (stream-car s) (force (car s)))



(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 10))







(define (map-stream proc str)
  (cond
    ((stream-null? (stream-cdr str)) (proc (stream-car str)))
    (else (proc (stream-car str)) (map-stream proc (stream-cdr str)))))


;(map-stream (lambda (x) (display x)) fibs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;str. oko 500, 506 i sl.

(define (eval expr env)
  (cond
    ((self-evaluating? expr) env)
    ((variable? expr) (lookup-variable-value expr env))
    ((quoted? expr) (text-of-quotation expr))
    ((assignment? expr) (eval-assignment expr env))
    ((if? expr) (eval-if expr env))
    ((lambda? expr) (make-procedure (lambda-parameters expr)
                                    (lambda-body expr)
                                    env))
    ((begin? expr) (eval-sequence (begin-actions expr) env))
    ((cond? expr) (eval (cond->if expr) env))
    ((application? expr) (apply (eval (operator expr) env)
                                (list-of-values (operands expr) env)))
    (else (error "Bravo."))))

(define (apply procedure arguments)
  (cond
    ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
     (eval-sequence (procedure-body procedure)
                    (extend-environment (procedure-parameters procedure)
                                        arguments
                                        (procedure-environment procedure))))
    (else (error "Bravo 2."))))


(define (list-of-values exps env)
  (if (no-operands? exps) '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (eval-if expr env)
  (if (true? (eval (if-predicate expr) env))
      (eval (if-consequent expr) env)
      (eval (if-alternative expr) env)))

(define (true? x) x)
(define if-predicate car)
(define if-consequent cadr)
(define (if-alternative exp)
  (if (not (null? (cdddr exp))
           (cadddr exp) #f)))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (eval-squence exps env)
  (cond
    ((last-exp? exps)
     (eval (first-exp exps) env))
    (else (eval (first-exp exps) env))))

(define (last-exp? exp) (null? (cdr exp)))
(define first-exp car)
(define rest-exps cdr)



(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (assignenment? exp) (tagged-list? exp 'set!))
(define assignment-variable cadr)
(define assignment-value caddr)


(define (application? exp) (pair? exp))
(defien operator car)
(define operands cdr)

(define (sequence->expression seq)
  (cond
    ((null? seq) seq)
    ((last-expr? seq) (first-exp seq))
    (else (make-begin 
                     
    


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp)  env)
    env))

;(define assignment-variable cadr)
;(define assignment-value caddr)


(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
       
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caadr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define lambda-parameters cadr)
(define lambda-body cddr)
(define (make-lambda parameters body)
  (cosn 'lambda (cons parameters body)))


(define (self-evaluating? expr)
  (cond
    ((number? expr) #t)
    ((string? expr) #t)
    (else #f)))

(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quoted))

(define text-of-quotations cadr)

(define (tagged-list? exp tag)
  (if (pair? exp) (eq? (car exp ) tag) #f))


(define (setup-environment) 'nope)
(define (compound-procedure? expr) #f)

(define (procedure-parameters proc) '())
(define (procedure-body proc) '())


(define input-prompt ">>> M-Eval input :")
(define output-prompt ">>> M-Eval value :")


(define (drive-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
        (let ((output (eval input the-global-environment)))
          (announce-output output-prompt))
    (drive-loop)))
          
        
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))


(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (the-global-environment) (setup-environment))
























































