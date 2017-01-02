(define (assign sub var shape) `((,var ,shape) . ,sub))

(define (lookup sub var)
  (match sub
    (`((,v ,shape) . ,sub2)
      (if (eq? v var)
        shape
        (lookup sub2 var)))))





