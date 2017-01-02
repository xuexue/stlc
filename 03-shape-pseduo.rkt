(define (assign sub var shape) `((,var ,shape) . ,sub))

(define (lookup sub var)
  (match sub
    (`((,v ,shape) . ,sub2)
      (if (eq? v var)
        shape
        (lookup sub2 var)))
    (_ #f)))


;(list `const `dfsdf)
;(list `var `sdfsfds)
;(list (list `const `dfsdf) `-> (list `const `dfsdf2))

;->
(walk sub (list `var `A))
(lookup sub (list `var `B))

(define (walk sub variable)
  (match variable
    (`(const ,x) ; immediate return if it's a concrete shape
      `(const ,x)) ; variable
    (`(,x -> ,y) ; walk either side
      (list (walk sub x) `-> (walk sub y)))
    (`(var ,x) ; substitute the inner variable
      (match (lookup sub variable)
        (#f variable)
        (z (walk sub z))))))


(define (does-not-occur? shape var)
  (match shape
    (`(const ,x) #t)
    (`(var ,x) (not (equal? x var)))
    (`(,x -> ,y) 
      (and (does-not-occur? x var) (does-not-occur? y var)))))

(define (assign? sub var shape)
  (if (does-not-occur? shape var) (assign sub var shape) #f))

(define (unify sub lhsuw rhsuw)
  (match (sub)
    (#f #f)
    (_
      (let ((lhs (walk sub lhsuw)) (rhs (walk sub rhsuw)))
          (match (list lhs rhs)
            (`((var ,x) (var ,y))
              (if (equal? x y) sub (assign sub rhs lhs)))
            (`((var ,x) _)
              (assign? sub lhs rhs))
            (`(_ (var ,x))
              (assign? sub rhs lhs))
            (`((const ,x) (const ,x))
              sub)
            (`((,a -> ,b) (,c -> ,d))
              (unify (unify sub b d) a c))
            (_ #f)
          )
      )
    )
  )
)



`(a (x ,y) (x ,y))
