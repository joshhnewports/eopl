#lang eopl

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

;; 2.5
(define empty-env
  (lambda () '()))

(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (let ((saved-var (caar env))
              (saved-val (cdar env))
              (saved-env (cdr env)))
          (if (eqv? search-var saved-var)
              saved-val
              (apply-env saved-env search-var))))))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

;; 2.6
;; Env = () | (Var SchemeVal Env)
(define empty-env
  (lambda () '()))

(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (let ((saved-var (car env))
              (saved-val (cadr env))
              (saved-env (caddr env)))
          (if (eqv? search-var saved-var)
              saved-val
              (apply-env saved-env search-var))))))

(define extend-env
  (lambda (var val env)
    (list var val env)))

;; Env = (Var-list Val-list)
;; Var-list = () | (Var Var-list)
;; Val-list = () | (Val Val-list)
(define empty-env
  (lambda () '(() ())))

(define apply-env
  (lambda (env search-var)
    (scan (car env) (cadr env) search-var)))

(define scan
  (lambda (vars vals search-var)
    (cond ((null? vars)
           (report-no-binding-found search-var))
          ((eqv? (car vars) search-var)
           (car vals))
          (else (scan (cdr vars) (cdr vals) search-var)))))

(define extend-env
  (lambda (var val env)
    (list (cons var (car env))
          (cons val (cadr env)))))

;; Env = (Var-list . Val-list)
;; Var-list = Listof(Var)
;; Val-list = Listof(SchemeVal)
(define empty-env
  (lambda () '(())))

(define extend-env
  (lambda (var val env)
    (cons (cons var (car env))
          (cons val (cdr env)))))

(define apply-env
  (lambda (env search-var)
    (app-env (car env) (cdr env) search-var)))

(define app-env
  (lambda (vars vals search-var)
    (cond ((null? vars)
           (report-no-binding-found search-var))
          ((eqv? (car vars) search-var)
           (car vals))
          (else (app-env (cdr vars) (cdr vals) search-var)))))

;; 2.7
(define apply-env
  (lambda (env search-var)
    (app-env env search-var env)))

(define app-env
  (lambda (env search-var e)
    (cond ((eqv? (car env) ’empty-env)
           (report-no-binding-found search-var))
          ((eqv? (car env) ’extend-env)
           (let ((saved-var (cadr env))
                 (saved-val (caddr env))
                 (saved-env (cadddr env)))
             (if (eqv? search-var saved-var)
                 saved-val
                 (app-env saved-env search-var e))))
          (else (report-invalid-env e)))))

;; 2.8
(define empty-env?
  (lambda (env)
    (null? env)))

;; 2.9
(define has-binding?
  (lambda (env s)
    (if (null? env)
        #f
        (let ((saved-var (caar env))
              (saved-env (cdr env)))
          (if (eqv? s saved-var)
              #t
              (has-binding? saved-env s))))))

;; 2.10
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env (car vars)
                    (car vals)
                    (extend-env* (cdr vars)
                                 (cdr vals)
                                 env)))))

;; 2.11
(define empty-env
  (lambda () '()))

(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (let ((saved-vars (caar env))
              (saved-vals (cdar env))
              (saved-env (cdr env)))
          (let ((val (apply-env-in-rib saved-vars saved-vals search-var)))
            (if val
                val
                (apply-env saved-env search-var)))))))

(define apply-env-in-rib
  (lambda (vars vals search-var)
    (cond ((null? vars) #f)
          ((eqv? (car vars) search-var) (car vals))
          (else (apply-env-in-rib (cdr vars) (cdr vals) search-var)))))

(define extend-env
  (lambda (var val env)
    (cons (cons (list var) (list val)) ; ((var) val)
          env)))

(define extend-env*
  (lambda (vars vals env)
    (cons (cons vars vals) ; ((var ... var) val ... val)
          env)))

;; 2.12
;; Stack = Var x Stack -> Stack
(define empty-stack
  (lambda ()
    (lambda (observer)
      (if (eqv? observer 'empty?)
          #t
          (report-stack-is-empty observer)))))

(define push
  (lambda (val stack)
    (lambda (observer)
      (cond ((eqv? observer 'pop) stack)
            ((eqv? observer 'top) val)
            (else #f))))) ; empty-stack?

(define pop
  (lambda (stack)
    (stack 'pop)))

(define top
  (lambda (stack)
    (stack 'top)))

(define empty-stack?
  (lambda (stack)
    (stack 'empty?)))

(define report-stack-is-empty
  (lambda (observer)
    (eopl:error 'empty-stack "Called ~s on an empty stack"
                observer)))

;; 2.13
(define empty-env
  (lambda ()
    (list (lambda (search-var)                    ; apply-env
            (report-no-binding-found search-var)) 
          (lambda ()                              ; empty-env?
            #t))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list (lambda (search-var)                     ; apply-env
            (if (eqv? search-var saved-var)
                saved-val
                (apply-env saved-env search-var))) 
          (lambda ()                               ; empty-env?
            #f))))

(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define empty-env?
  (lambda (env)
    ((cadr env))))

;; 2.14
(define empty-env
  (lambda ()
    (list (lambda (search-var)                    ; apply-env
            (report-no-binding-found search-var))
          (lambda ()                              ; empty-env?
            #t)
          (lambda (s)                             ; has-binding?
            #f))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list (lambda (search-var)                     ; apply-env
            (if (eqv? search-var saved-var)
                saved-val
                (apply-env saved-env search-var)))
          (lambda ()                               ; empty-env?
            #f)
          (lambda (s)                              ; has-binding?
            (if (eqv? s saved-var)
                #t
                (has-binding? saved-env s))))))

(define has-binding?
  (lambda (env s)
    ((caddr env) s)))