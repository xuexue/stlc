#lang racket

(define (env-lookup env name)
  (match env
    (`((,n ,val) . ,env)
      (if (eq? n name)
        val                      ;; Reference binding
        (env-lookup env name)))  ;; Traverse environment
    ('() (error (format "unbound variable: ~a" name)))))

(define (eval-term env term)
  (match term
    ((? symbol?) (env-lookup env term))
    (`(lambda ,name ,body)
      `(closure ,name ,body ,env))  ;; Close over environment

(define (eval term) (eval-term '() term))
