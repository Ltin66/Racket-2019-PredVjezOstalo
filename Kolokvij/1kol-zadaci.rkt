#lang sicp


; 1. ZADATAK

; Za 1 1 1 1 prva 4 (x0 - x3)
; rez : 5, 13, 25, 53
; (foo-rek 1 1 1 1 4) , x4 je 5

; ekspanzija -> <- redukcija
(define (foo-rek x0 x1 x2 x3 n)
  (cond
    ([eq? n 0] x0)
    ([eq? n 1] x1)
    ([eq? n 2] x2)
    ([eq? n 3] x3)
    ([eq? n 4] (- (+ (* 2 x3) (* 3 x1) x0) x2))
    (else (- (+ (* 2 (foo-rek x0 x1 x2 x3 (- n 1)))
                (* 3 (foo-rek x0 x1 x2 x3 (- n 3)))
                (foo-rek x0 x1 x2 x3 (- n 4)))
             (foo-rek x0 x1 x2 x3 (- n 2))))))


          
           ;(foo-rek-rel x0 x1 x2 x3 (- n 4))
           ;(foo-rek-rel x0 x1 x2 x3 (- n 3))
           ;(foo-rek-rel x0 x1 x2 x3 (- n 2))
           ;(foo-rek-rel x0 x1 x2 x3 (- n 1))
           ;(- n 1)))))
           
;(define (foo-iter x0 x1 x2 x3 n)
;  (let ((x4 (- (+ (* 2 x3) (* 3 x1) x0) x2)) )
;    (cond
;      ([eq? (- n 3) 1]  x4)
;      (else (foo-iter x1 x2 x3 x4 (- n 1))))))


; 2. ZADATAK

(define (foo-iter x0 x1 x2 x3 n)
  (define (foo-iter-iter x0 x1 x2 x3 n )
    (let ((x4 (- (+ (* 2 x3) (* 3 x1) x0) x2)) )
      (cond
        ([eq? n 1]  x4)
        (else (foo-iter-iter x1 x2 x3 x4 (- n 1))))))
  (foo-iter-iter x0 x1 x2 x3 (- n 3)))





;Map-, filter-, length-, count-, reverse-, accumulate-, flatmap
;member, find? (dali treba poz elem?)






;MAP
;apply proc to every elem
(define (map proc lst)
  (cond
    ((null? lst) '())
    (else (cons (proc (car lst)) (map proc (cdr lst))))))


;FILTER
;return list of elem if predicate? #t for elem
(define (filter pred? lst)
  (cond
    ([null? lst] '())
    ([pred? (car lst)] (cons (car lst) (filter pred? (cdr lst)))) 
    (else (filter pred? (cdr lst)))))

;(filter odd? '(1 2 3 4 5 6 7 8 9))



;LENGTH
;ret len of lst
(define (length lst)
  (define (len-iter lst cntr)
    (cond
      ([null? lst] cntr)
      (else (len-iter (cdr lst) (+ 1 cntr)))))
  (len-iter lst 0))
    

;REVERSE
(define (reverse lst)
  (if [null? lst] '()
      (append (reverse (cdr lst)) (list (car lst)))))
;              na kraju (rekurzije) vraca praznu listu
;              pa vraca zadnji elem spojen sa praznom listom
;              pa vraca listu sa zadnjim elem spojenu sa predzadnjim
;                                   ....
;              vraca reversanu listu na koju dodaje prvi elem na kraj



;COUNT
; returns # of #t for pred? on elem
(define (count predicate? lst)
  (cond
    ([null? lst] 0)
    ([predicate? (car lst)] (+ 1 (count predicate? (cdr lst))))
    (else (+ (count predicate? (cdr lst)) 0))))
  

;> (count odd? '(1 2 3 4 5 6 9))
;4

(define (sum-list init lst)
  (accumulate (lambda (x y) (+ x y)) init lst))

  
;Accumulate,FoldR,Reduce,Compress,Inject
(define (accumulate proc init lst)
  (cond
    ([null? lst] init)
    (else (proc (car lst) (accumulate proc init (cdr lst))))))
  
(sum-list 0 '(1 2 3 4 5))


;MEMBER
;if elem in lst #t els #f

(define (member elem lst)
  (cond
    ([null? lst] #f)
    ([eq? elem (car lst)] #t)
    (else (member elem (cdr lst)))))

(member 3 '(1 2 3 4 5))


(define member? member)


;3. ZADATAK



(define (unique lst)
  (define (uniquer lst lst-un)
    (cond
      ([null? lst] lst-un)
      ([not (member (car lst) lst-un)]
       (uniquer (cdr lst) (append lst-un (list (car lst)))))
      (else (uniquer (cdr lst) lst-un))))        
  (uniquer lst '()))

(unique '(1 1 2 3 4 3 5 3 2 1))



; 4. ZADATAK

(define (add1 x) (+ 1 x))

(append '( '(2 . 1) '(3 . 2) ) (list (cons (add1 4) 4)))



(define (map-with-original proc lst)
  (define (map proc lst lst-tmp)
    (cond
      ([null? lst] lst-tmp)
      (else (map proc (cdr lst)
                 (append lst-tmp (list (cons (proc (car lst))
                                             (car lst))))))))
  (map proc lst '()))
  


;(map-with-original add1 '(1 2 3 4))




;5. ZADATAK

(define (get-group elem lst)
  (define (group elem lst lst-tmp)
    (cond
      ([null? lst] lst-tmp)
      ([eq? elem (car (car lst))] (group elem
                                         (cdr lst)
                                         (append lst-tmp (list (cdr (car lst))))))
      (else (group elem (cdr lst) lst-tmp))))
  (group elem lst '()))


(get-group 'a '( (a . c) (b . i) (a . z) (f . d)))



; 6. ZADATAK , pitat kako znat koji adresirat


; 7. ZADATAK

(define (group-by pred? lst)
  (define (groupy pred? lst lst-t lst-f)
    (cond
      ([null? lst] (list lst-t lst-f))
      ([pred? (car lst)] (groupy pred? (cdr lst) (append lst-t (list (car lst))) lst-f))
      (else (groupy pred? (cdr lst) lst-t (append lst-f (list (car lst)))))))
  (groupy pred? lst '() '()))

      
(group-by odd?  '(1 2 3 4 5))




;FLATTEN
;

(define (flatten lst)
  (cond
    ([null? lst] '())
    ([pair? (car lst)]
     (append (flatten (car lst))
             (flatten (cdr lst))))
    (else (cons (car lst) (flatten (cdr lst))))))

(flatten '(9 (1 3 5) 3 (2 (1 2 3 4))))



;FLATMAP

(define (flatmap proc seq) 
  (accumulate append 0 (map proc seq)))


(map add1 '(1 2 3 4 5 6 7 8 9) )


(accumulate (lambda (x y) (cons (cons x y) y)) '() '(1 2 3 4 5))

(define (lesser x y)  (if(< x y) x y))

(define (min lst) (accumulate lesser
                              (if(null? lst) '() (car lst)) lst))

































  