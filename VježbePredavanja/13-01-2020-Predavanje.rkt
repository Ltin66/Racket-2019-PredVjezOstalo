
#lang sicp

;str 345, 347,353,359,concurency (10/10 m8 must read), oko 430


(define (append x y)
  (if (null? x) y
      (cons (car x) (append (cdr x) y))))


;koristimo helper funckije kako bi znali da abstraktamo od nečeg (primitivnijih operacija valjda)

(define front-ptr car)
(define rear-ptr cdr)

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))


(define (make-queue) (cons '() '()))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Bravo.")
      (car (front-ptr queue))))


(define (insert-queue! queue item)
        (let ((new-pair (cons item '())))
          (cond ((empty-queue?)
                 (set-front-ptr! queue new-pair)
                 (set-rear-ptr! queue new-pair)
                 queue)
                (else
                 (set-cdr! (rear-ptr queue) new-pair)
                 (set-rear-ptr! queue new-pair)))))

(define (delete-queue! queue item)
  (cond ((empty-queue? queue)
         (error "Bravo."))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))





(define(lookup key table)
  (let ((record (assoc key(cdr table))))
    (if record (cdr record) false)))

(define(assoc key records)
  (cond
    ((null? records) false)
    ((equal? key(caar records))
     (car records))
    (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc  key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons  key value)
                        (cdr table)))))
  'ok)

(define (make-table) (list '*table*))



;streams are delayed lists
;bla bla bla
;da zauzima manje memorije, umjesto cijele liste, uzimamo elem po elem

;delay of an S-expression = (lambda () (S-expression)) str. 434
;reverse of delay is "force"




(define (stream-cons a b) (cons a (delay b)))

(define (stream-cdr s) (force (cdr s)))
(define (stream-car s) (force (car s)))


;(define 

;(define (stream-ref s n)
 ; (if (= n 0)
  ;    (stream-car s)
   ;   (stream-ref (stream-cdr s) (- n 1))))


;(define (stream-map proc init)
  

;(stream-cons (+ 1 2) (/ 1 0))

;(force (cdr (cons-stream (+ 1 2) (/ 1 0))))




(define (enumerate-interval a b)
  (if (a . > . b) '()
      (cons a (enumerate-interval (+ a 1) b))))


(enumerate-interval 1 5)



;(enumerate-interval 1 1000000000)


(define (stream-enumerate-interval a b)
  (if (a . > . b) '()
      (cons-stream a (stream-enumerate-interval (+ a 1) b))))


(stream-enumerate-interval 1 5)

(stream-cdr (stream-enumerate-interval 1 5))

;odmah se izvrsi jer je izvršavanje zakašnjeno,
;tek kad se bude trebalo izvršiti onda ce se izvrsiti,
;za razliku od normalnog enumertate-interval


(stream-cdr (stream-enumerate-interval 1 1000000000))



;str 441

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

;(display-stream integers) ;crash



;str 442 - skipping elements in a stream






(define ones (cons-stream 1 ones)) ;str 444.


;u četvrtak 4. poglavlje evaluacija programa





















