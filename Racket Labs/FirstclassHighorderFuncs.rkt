#lang racket

(define (convertFC temps)
  (map fcHelper temps))

(define (fcHelper temp)
  (*
   (- temp 32)
   (/ 5 9)))

(define (check-temps1 temps)
  (check-temps temps 5 95))

(define (check-temps temps low high)
  (if (empty? temps)
      #t
      (andmap
       (lambda (temps)
         (not (or
               (< temps low)
               (> temps high))))
       temps)))

(define (convert digits)
  (foldr
   (lambda (a b result) (+ a (* 10 result)))
   0
   digits
   digits))

(define (duple lst)
  (map
   (lambda (F) (list F F))
   lst))

(define (count-list-elems lst)
  (if(empty? lst)
     0
     (+ 1(count-list-elems(rest lst)))
     ))

(define (average lst)
  (/ (foldr + 0 lst)(count-list-elems lst)))

(define (smaller-than-all-following? lst val)
  (if(empty? lst)
     #t
     (if(> val (first lst))
        #f
        (smaller-than-all-following? (rest lst) val))))

(define (eliminate-larger lst)
  (cond
    [(empty? lst) empty]
    [else
     (define lowest(first (reverse lst)))
     (reverse (foldl
      (lambda (elem val)
        (cond
          [(> elem lowest) val]
          [else
           (define lowest elem)
           (append val(list elem))]))
      (list (first (reverse lst)))
      (rest(reverse lst))))]))

(define (curry2 func)
  (lambda (arg1)
    (lambda (arg2)
      (func arg1 arg2))))