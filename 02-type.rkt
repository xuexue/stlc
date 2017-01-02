#lang racket

(define (type-lookup env name)
  (match env
    (`((,n ,val) . ,env)
      (if (eq? n name)
        val                        ;; Reference binding
        (type-lookup env name))))) ;; Traverse environment

(define (type-extend gamma name val) `((,name ,val) . ,gamma))

(define (type-checker gamma term)
  (match term
    (`((lambda ,name ,body) : (,type1 -> ,type2))
      (let* ((gamma2 (type-extend gamma name type1))
             (type-of-result (type-checker gamma2 body)))
        (if (equal? type-of-result type2)
          `(,type1 -> ,type2)
          (error (format "expected type '~a' but found '~a'"
                         type2 type-of-result)))))
    (`(,fn ,arg)
      (match (type-checker gamma fn)
        (`(,fnin -> ,fnout)
          (let ((type-of-arg (type-checker gamma arg)))
            (if (equal? type-of-arg fnin)
              fnout
              (error (format "expected type '~a' but found '~a'"
                             fnin type-of-arg)))))))
    (_ (type-lookup gamma term))))

; gamma { c: (1->2), n: 1 }

;x = (lambda (c n) (lambda c c) (lambda c c))
;g = ((n `(1 -> 1)) . (c `(1 -> 1)))
;;(type-checker g x)
;(type-checker g `(,term : ,type) ...)

;x = [(lambda (c n) [(lambda c c): T] [(lambda c c): T]) : T]


;c is a function [1 -> 2]
;n is an inert crap [1]
;inner lambda [1 -> 2]
;outer lambda [(1 -> 2) -> (1 -> 2)]

;(lambda (c n) (c n)) ;; (lambda c (lambda n (c n))[1->2] ) [(1->2) -> (1->2)]

;g = ((n `1) . (c `(1 -> 2)))
;output => `((1 -> 2) -> 1 -> 2)
