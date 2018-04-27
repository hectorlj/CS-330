#lang racket

(define (default-parms f values)
  (lambda args
    (if(eqv?(length args)(length values))
         (apply f args)
         (apply f
                (append args
                          (list-tail values
                                     (length args)))))))


(define (type-parms f types)
  (lambda args
    (apply f
           (map (lambda (pairing)
                  (if
                   (not ((second pairing) (first pairing)))
                      (error "got an error")
                      (first pairing)))
                (zip args types)))))

(define zip
  (lambda (lst1 lst2)
    (map list lst1 lst2)))

(define (convert angle)
  (* (/ angle 180) pi))

(define(newSin angle measure)
  (if
   (symbol=? measure 'degrees)
     (sin (convert angle))
     (sin angle)))

(define new-sin2 (default-parms
                   (type-parms newSin
                               (list number? symbol?))
                   (list 0 'radians)))
