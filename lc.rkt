#lang racket

(define (eval-term env name)
  (match env
    (`((,n ,val) . ,env)
      (if (eq? n name)
        val                     ;; Reference binding
        (eval-term env name)))  ;; Traverse environment
    ('() (error (format "unbound variable: ~a" name)))))

(define (eval term) (eval-term '() term))
