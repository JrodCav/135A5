;Jarod Cavagnaro
;Assignment 5
;CSC 135

;a)
(define (inc_n n)
  (lambda (x)
    (+ n x)
))

;b)
(define len
  (lambda (L)
    (if (null? L) 0
        (+ 1 (len (cdr L)))
)))

;c)
(define maxx
  (lambda (L)
    (cond
      ((null? L) 0)
      ((null? (cdr L)) (car L))
      ((> (car L) (maxx(cdr L))) (car L))
      (else (maxx (cdr L)))
)))
(define minn
  (lambda (L)
    (cond
      ((null? L) 0)
      ((null? (cdr L)) (car L))
      ((< (car L) (minn(cdr L))) (car L))
      (else (minn(cdr L)))
)))
(define maxmin
  (lambda (L)
    (list(maxx L) list(minn L))
))


;d)
(define (mem x L)
    (cond
      ((null? L) #f)
      (else (if (equal? x (car L)) #t
                (mem x (cdr L))
))))

;e)
(define (ins x L)
  (if (not (mem x L)) (append (list x)L)
))

;f)
(define (numT b L)
  (cond
    ((null? L) 0)
    ((if (b(car L)) (+ 1 (numT b(cdr L)))
         (+ 0 (numT b(cdr L)))
))))

;g
(define (moreT b L K)
  (cond
    ((> (numT b L) (numT b K)) 1)
    ((< (numT b L) (numT b K)) 2)
    (else 0)
))