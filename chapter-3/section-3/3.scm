#lang eopl

;; 3.19
;; Expression ::= letproc Identifier = (Identifier) Expression in Expression
;; expressions
(letproc-exp
 (name identifier?)
 (var identifier?)
 (proc-body expression?)
 (body expression?))

;; value-of
(letproc-exp (name var proc-body body)
             (value-of body
                       (extend-env name
                                   (proc-val (procedure var
                                                        proc-body
                                                        env))
                                   env)))

;; 3.20
;; proc (x) proc (y) -(x, -(0, y))

;; 3.21
;; does not handle arity mismatch or procedures that take zero arguments
;; procedure representation
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))

(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (value-of body (extend-env* vars
                                             vals
                                             saved-env))))))

;; expressions
(proc-exp
 (formal-parameters (list-of identifier?))
 (body expression?))
(call-exp
 (rator expression?)
 (rands (list-of expression?)))

;; value-of
(proc-exp (params body)
          (proc-val (procedure params body env)))
(call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of rand env))
                           rands)))
            (apply-procedure proc args)))

;; 3.22
;; (- Expression, Expression)
;; (zero? Expression)

;; 3.23
(times 4 3)
((makemult makemult) 3)
-(((makemult makemult) 2), -4)
-(-(((makemult makemult) 1), -4), -4)
-(-(-(((makemult makemult) 0), -4), -4), -4)
-(-(-(0, -4), -4), -4)
-(-(4, -4), -4)
-(8, -4)
12

let times = proc (maker)
             proc (y)
              proc (x)
               if zero?(x)
               then 0
               else -((((maker maker) y) -(x,1)), -(0, y))
in let fact = proc (maker)
               proc (x)
                if zero?(x)
                then 1
                else (((times times) ((maker maker) -(x, 1))) x)
in ((fact fact) 3)

((fact fact) 3)
(((times times) ((fact fact) 2))
 3)

(((times times)
  (((times times) ((fact fact) 1))
   2))
 3)

(((times times)
  (((times times)
    (((times times) ((fact fact) 0))
     1))
   2))
 3)

(((times times)
  (((times times)
    (((times times) 1) 1))
   2))
 3)


(((times times)
  (((times times)
    (((times times) 1) 1))
   2))
 3)

(((times times)
  (((times times)
    -((((times times) 1) 0), -1)) 2))
 3)

(((times times)
  (((times times) 1) 2))
 3)

(((times times)
  -((((times times) 1) 1), -1))
 3)

(((times times)
  -(-((((times times) 1) 0), -1), -1))
 3)

(((times times)
  -(-(0, -1), -1))
 3)

(((times times) 2) 3)

-((((times times) 2) 2), -2)

-(-((((times times) 2) 1), -2), -2)

-(-(-((((times times) 2) 0), -2), -2), -2)

-(-(-(0, -2), -2), -2)

-(-(2, -2), -2)
-(4, -2)
6

;; 3.24
(define odd
  (lambda (this)
    (lambda (next)
      (lambda (x)
        (if (zero? x)
            0
            (((next next) this) (- x 1)))))))

(define even
  (lambda (this)
    (lambda (next)
      (lambda (x)
        (if (zero? x)
            1
            (((next next) this) (- x 1)))))))

let makeeven = proc (this)
                proc (next)
                 proc (x)
                  if zero?(x)
                  then 1
                  else (((next next) this) -(x, 1))
in let makeodd = proc (this)
                  proc (next)
                   proc (x)
                    if zero?(x)
                    then 0
                    else (((next next) this) -(x, 1))
in let odd = proc (x) (((makeodd makeodd) makeeven) x)
in let even = proc (x) (((makeeven makeeven) makeodd) x)
in (even a)

;; 3.25
(times4 3)
((makerec maketimes4) 3)

((maketimes4 (d d)) 3)
((maketimes4 proc (z) ((maketimes4 (d d)) z)) 3)
-((proc (z) ((maketimes4 (d d)) z) 2), -4)

-(((maketimes4 (d d)) 2), -4)
-(((maketimes4 proc (z) ((maketimes4 (d d)) z)) 2), -4)
-(-((proc (z) ((maketimes4 (d d)) z) 1), -4), -4)

-(-(((maketimes4 (d d)) 1), -4), -4)
-(-(((maketimes4 proc (z) ((maketimes4 (d d)) z)) 1), -4), -4)
-(-(-((proc (z) ((maketimes4 (d d)) z) 0), -4), -4), -4)

-(-(-(((maketimes4 (d d)) 0), -4), -4), -4)
-(-(-(((maketimes4
        proc (z) ((maketimes4 (d d)) z)) 0), -4), -4), -4)
-(-(-(0, -4), -4), -4)
-(-(4, -4), -4)
-(8, -4)
12

;; 3.26
;; value-of
(proc-exp (var body)
          (proc-val
           (procedure
            var
            body
            (let ((vars (no-repeats (all-occurs-free body))))
              (extend-env* vars
                           (map (lambda (v) (apply-env env v))
                                vars)
                           (empty-env))))))

(define all-occurs-free
  (lambda (exp)
    (cases expression exp
      (const-exp (num) '())
      (var-exp (var) (list var))
      (diff-exp (exp1 exp2)
                (append (all-occurs-free exp1)
                        (all-occurs-free exp2)))
      (zero?-exp (exp1)
                 (all-occurs-free exp1))
      (if-exp (exp1 exp2 exp3)
              (append (all-occurs-free exp1)
                      (append (all-occurs-free exp2)
                              (all-occurs-free exp3))))
      (let-exp (var exp1 body)
               (append (all-occurs-free exp1)
                       (remove var (all-occurs-free body))))
      (proc-exp (var body)
                (remove var (all-occurs-free body)))
      (call-exp (rator rand)
                (append (all-occurs-free rator)
                        (all-occurs-free rand))))))

;; from 1.22
(define filter-in
  (lambda (pred lst)
    (cond ((null? lst) '())
          ((pred (car lst))
           (cons (car lst) (filter-in pred (cdr lst))))
          (else (filter-in pred (cdr lst))))))

(define remove
  (lambda (x lst)
    (filter-in (lambda (a) (not (eqv? x a)))
               lst)))

(define no-repeats
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (car lst) (no-repeats (remove (car lst) (cdr lst)))))))

;; 3.27
;; proc datatype
(traceproc
 (var identifier?)
 (body expression?)
 (saved-env environment?))

;; expressions
(traceproc-exp
 (var identifier?)
 (body expression?))

;; value-of
(traceproc-exp (var body)
               (proc-val (traceproc var body env)))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env)))
      (traceproc (var body saved-env)
                 (display "Enter")
                 (let ((val1 (value-of body (extend-env
                                            var
                                            val
                                            saved-env))))
                   (display "Exit")
                   val1)))))

;; 3.28
;; data structure representation
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)))

(define apply-procedure
  (lambda (proc1 val env)
    (cases proc proc1
      (procedure (var body)
                 (value-of body (extend-env var val env))))))

;; procedural representation
(define procedure
  (lambda (var body)
    (lambda (val env)
      (value-of body (extend-env var val env)))))

(define apply-procedure
  (lambda (proc arg env)
    (proc arg env)))

;; value-of
(proc-exp (var body) (proc-val (procedure var body)))
(call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg env)))