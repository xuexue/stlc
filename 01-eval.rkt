#lang racket

(define env-empty '())

(define (env-lookup-default env name default)
  (match env
    (`((,n ,val) . ,env)
      (if (eq? n name)
        val                                     ;; Reference binding
        (env-lookup-default env name default))) ;; Traverse environment
    ('() (default))))
(define (env-lookup env name) (env-lookup-default env name (lambda () name)))
(define (env-lookup-fail env name)
  (env-lookup-default
    env name (lambda () (error (format "unbound variable: ~a" name)))))

(define (env-extend env name val) `((,name ,val) . ,env))

;; Extended to support symbolic evaluation under unapplied lambdas
(define (eval-term env term)
  (match term
    (`(lambda ,name ,body)
      `(closure ,name ,body ,env))  ;; Close over environment
    (`(,fn ,arg)
      (match (eval-term env fn)
        (`(closure ,name ,body ,cenv)  ;; Apply closure
          (eval-term (env-extend cenv name (eval-term env arg)) body))
        (vfn `(,vfn ,(eval-term env arg)))))  ;; Symbolic application
    (_ (env-lookup env term))))

(define (normalize-term env term)
  (match (eval-term env term)
    (`(closure ,name ,body ,cenv)
      (let ((name1 (gensym name)))
        `(lambda ,name1 ,(normalize-term (env-extend cenv name name1) body))))
    (`(,fn ,arg) `(,(normalize-term env fn) ,(normalize-term env arg)))
    (term term)))

(define (eval term) (eval-term env-empty term))
(define (normalize term) (normalize-term env-empty term))
