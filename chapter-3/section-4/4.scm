#lang eopl

;; the val in an extend-env is an expval, so returning saved-val in apply-env is fine.
;; for an extend-env-rec, the fields must be gathered to make a proc, and then return an expval.

;; 3.31
;; expressions
(letrec-exp
 (p-name identifier?)
 (b-vars (list-of identifier?))
 (p-body expression?)
 (letrec-body expression?))
(call-exp
 (rator expression?)
 (rands (list-of expression?)))

;; small change
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val expval?)
   (env environment?))
  (extend-env-rec
   (p-name identifier?)
   (b-vars (list-of identifier?))
   (body expression?)
   (env environment?)))

;; small change
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))

;; value-of
(letrec-exp (p-name b-vars p-body letrec-body)
            (value-of letrec-body
                      (extend-env-rec p-name b-vars p-body env)))
(call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of rand env))
                           rands)))
            (apply-procedure proc args)))

;; to allow procedures as arguments, extend-env* would need to be modified as well
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (value-of body
                           (extend-env* vars
                                        vals
                                        saved-env))))))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env () (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
                  (if (eqv? saved-var search-var)
                      saved-val
                      (apply-env saved-env search-var)))
      (extend-env-rec (p-name b-vars p-body saved-env)
                      (if (eqv? search-var p-name)
                          (proc-val (procedure b-vars p-body env))
                          (apply-env saved-env search-var))))))

;; 3.32
;; expressions
(letrec-exp
 (p-names (list-of identifier?))
 (b-vars (list-of identifier?))
 (p-bodies (list-of expression?))
 (letrec-body expression?))

;; value-of
(letrec-exp (p-names b-vars p-bodies letrec-body)
            (value-of letrec-body
                      (extend-env-rec* p-names
                                       b-vars
                                       p-bodies
                                       env)))

(define extend-env-rec*
  (lambda (p-names b-vars p-bodies env)
    (if (null? p-names)
        env
        (extend-env-rec (car p-names)
                        (car b-vars)
                        (car p-bodies)
                        (extend-env-rec* (cdr p-names)
                                         (cdr b-vars)
                                         (cdr p-bodies)
                                         env)))))

;; 3.33
;; expressions
(letrec-exp
 (p-names (list-of identifier?))
 (list-b-vars (list-of (list-of identifier?))) ; list of parameters for each procedure
 (p-bodies (list-of expression?))
 (letrec-body expression?))
(call-exp
 (rator expression?)
 (rands (list-of expression?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val expval?)
   (env environment?))
  (extend-env-rec
   (p-name identifier?)
   (b-vars (list-of identifier?))
   (body expression?)
   (env environment?)))

(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))

;; value-of
(letrec-exp (p-names list-b-vars p-bodies letrec-body)
            (value-of letrec-body
                      (extend-env-rec* p-names
                                       list-b-vars
                                       p-bodies
                                       env)))
(call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of rand env))
                           rands)))
            (apply-procedure proc args)))

(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (value-of body
                           (extend-env* vars
                                        vals
                                        saved-env))))))

;; 3.34
(define extend-env-rec
  (lambda (p-name b-var body env)
    (lambda (search-var)
      (if (eqv? search-var p-name)
          (proc-val (procedure b-var body env))
          (apply-env env search-var)))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

;; 3.35
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val (or vector? expval?))
   (env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env () (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
                  (if (vector? saved-val)
                      (vector-ref saved-val 0)
                      (if (eqv? saved-var search-var)
                          saved-val
                          (apply-env saved-env search-var)))))))

;; 3.36
;; the same as 3.32
;; expressions
(letrec-exp
 (p-names (list-of identifier?))
 (b-vars (list-of identifier?))
 (p-bodies (list-of expression?))
 (letrec-body expression?))

;; value-of
(letrec-exp (p-names b-vars p-bodies letrec-body)
            (value-of letrec-body
                      (extend-env-rec* p-names
                                       b-vars
                                       p-bodies
                                       env)))

;; 3.37
;; under dynamic binding, (fact 5) is called in an environment where fact is defined,
;; therefore calls to *(n, (fact -(n,1))) occur in an environment where fact is defined

;; under lexical binding, calls to *(n, (fact -(n,1))) occur in an environment where
;; fact is defined as proc (n) add1(n)

let even(x) = if zero?(x) then 1 else (odd -(x,1))
in let odd(x) = if zero?(x) then 0 else (even -(x,1))
in (odd 3)

;; even calls (odd -(x,1)) in an environment extended from the point of call (even -(x,1)), which
;; is itself extended from the point of call (odd 3), which has as its environment the definitions
;; odd and even.
