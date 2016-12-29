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

(define (denote-term term)
  (match term
    (`(closure ,name ,body ,cenv)
      (let ((clo ((denote-term `(lambda ,name ,body))
                  (map (lambda (binding)
                         (list (car binding) (list #f (cadr binding))))
                       cenv))))
        (lambda (env) clo)))
    (`(lambda ,name ,body)
      (let ((dbody (denote-term body)))
        (lambda (env)
          (lambda (arg) (dbody (env-extend env name (list #t arg)))))))
    (`(,fn ,arg)
      (let ((dfn (denote-term fn)) (darg (denote-term arg)))
        (lambda (env) ((dfn env) (darg env)))))
    (_ (lambda (env)
         (match (env-lookup-default env term (lambda () (list #t term)))
           (`(#t ,val) val)
           (`(#f ,term) ((denote-term term) env)))))))

(define (eval term) (eval-term env-empty term))
(define (normalize term) (normalize-term env-empty term))
(define (denote term) ((denote-term term) env-empty))

(define (closure->boolean clo) ((clo #t) #f))
(define (closure->nat clo) ((clo add1) 0))
(define (closure->pair left right clo)
  (clo (lambda (h) (lambda (t) (cons (left h) (right t))))))
(define (closure->list element clo)
  (map element ((clo (lambda (h) (lambda (t) (cons h t)))) '())))

(define (denote-boolean term) (closure->boolean (denote term)))
(define (denote-nat term) (closure->nat (denote term)))
(define (denote-pair left right term)
  (closure->pair left right (denote term)))
(define (denote-list element term) (closure->list element (denote-term)))

(define (parse penv stx)
  (define (free-name? name)
    (not (env-lookup-default penv name (lambda () #f))))
  (define (special? name)
    (procedure? (env-lookup penv name)))
  (match stx
    (`(,(? free-name? 'lambda) ,params ,body)
      (match params
        (`(,name)          (parse penv `(lambda ,name ,body)))
        (`(,name . ,names) (parse penv `(lambda ,name (lambda ,names ,body))))
        (name              `(lambda ,name
                              ,(parse (env-extend penv name name) body)))))
    (`(,(? symbol? (? special? fn)) . ,args)
      (parse penv (apply (env-lookup penv fn) args)))
    (`(,fn . ,args)
      (let loop ((args args) (pfn (parse penv fn)))
        (match args
          (`(,arg . ,args)
            (loop args `(,pfn ,(parse penv arg))))
          ('() pfn))))
    (_ (env-lookup penv stx))))
