#lang eopl

;; 3.6
;; add to expression data types
(minus-exp (exp1 expression?))

;; add case to value-of
(minus-exp (exp1)
           (value-of (diff-exp (const-exp 0)
                               exp1)
                     env))

;; 3.7
;; add to expression data types
(add-exp
 (exp1 expression?)
 (exp2 expression?))
(mul-exp
 (exp1 expression?)
 (exp2 expression?))
(div-exp
 (exp1 expression?)
 (exp2 expression?))

;; add these cases to value-of
(add-exp (exp1 exp2)
         (num-val (+ (expval->num (value-of exp1 env))
                     (expval->num (value-of exp2 env)))))
(mul-exp (exp1 exp2)
         (num-val (* (expval->num (value-of exp1 env))
                     (expval->num (value-of exp2 env)))))
(div-exp (exp1 exp2)
         (let ((val2 (expval->num (value-of exp2 env))))
           (if (= 0 val2)
               (report-division-by-zero)
               (num-val (/ (expval->num (value-of exp1 env))
                           val2)))))

;; 3.8
;; expressions
(equal?-exp
 (exp1 expression?)
 (exp2 expression?))
(greater?-exp
 (exp1 expression?)
 (exp2 expression?))
(less?-exp
 (exp1 expression?)
 (exp2 expression?))

;; value-of
(equal?-exp (exp1 exp2)
            (bool-val (= (expval->num (value-of exp1 env))
                         (expval->num (value-of exp2 env)))))
(greater?-exp (exp1 exp2)
              (bool-val (> (expval->num (value-of exp1 env))
                           (expval->num (value-of exp2 env)))))
(less?-exp (exp1 exp2)
           (bool-val (< (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))

;; 3.9
;; ExpVal = Int + Bool + List + Emptylist
;; DenVal = Int + Bool + List + Emptylist
;; expvals
(emptylist-val)
(list-val
 (first expval?)
 (rest expval?))

(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (first rest) (cons first rest))
      (else (report-expval-extractor-error 'list val)))))

;; expressions
(cons-exp
 (exp1 expression?)
 (exp2 expression?))
(car-exp
 (exp1 expression?))
(cdr-exp
 (exp1 expression?))
(null?-exp
 (exp1 expression?))
(emptylist-exp)

;; value-of
(cons-exp (exp1 exp2)
          (list-val (value-of exp1 env)
                    (value-of exp2 env)))
(car-exp (exp1)
         (car (expval->list (value-of exp1 env))))
(cdr-exp (exp1)
         (cdr (expval->list (value-of exp1 env))))
(null?-exp (exp1)
           (let ((val1 (value-of exp1 env)))
             (cases expval val1
               (emptylist-val () (bool-val #t))
               (else (bool-val #f)))))
(emptylist-exp ()
               (emptylist-val))

;; 3.10
;; expressions
(list-exp
 (exps (list-of expression?))) ; list-of works well with map

;; value-of
;; transform a scheme list of expvals to a nested list-val with emptylist-val at the end
(list-exp (exps)
          (list->expval (map (lambda (expr) (value-of expr env))
                             exps)))

;; Listof(ExpVal) -> ExpVal
(define list->expval
  (lambda (lst)
    (if (null? lst)
        (emptylist-val)
        (list-val (car lst)
                  (list->listval (cdr lst))))))

;; 3.11
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (const-op num))
      (var-exp (var) (var-op var env))
      (diff-exp (exp1 exp2) (diff-op exp1 exp2 env))
      (zero?-exp (exp1) (zero?-op exp1 env))
      (if-exp (exp1 exp2 exp3) (if-op exp1 exp2 exp3 env))
      (let-exp (var exp1 body) (let-op var exp1 body env))
      (minus-exp (exp1) (minus-op exp1 env))
      (add-exp (exp1 exp2) (add-op exp1 exp2 env))
      (mul-exp (exp1 exp2) (mul-op exp1 exp2 env))
      (div-exp (exp1 exp2) (div-op exp1 exp2 env))
      (equal?-exp (exp1 exp2) (equal?-op exp1 exp2 env))
      (greater?-exp (exp1 exp2) (greater?-op exp1 exp2 env))
      (less?-exp (exp1 exp2) (less?-op exp1 exp2 env))
      (cons-exp (exp1 exp2) (cons-op exp1 exp2 env))
      (car-exp (exp1) (car-op exp1))
      (cdr-exp (exp1) (cdr-op exp1))
      (null?-exp (exp1) (null?-op exp1 env))
      (emptylist-exp () (emptylist-op))
      (list-exp (exps) (list-op exps env)))))

(define const-op
  (lambda (num)
    (num-val num)))

(define var-op
  (lambda (var env)
    (apply-env env var)))

(define diff-op
  (lambda (exp1 exp2 env)
    (let ((val1 (value-of exp1 env))
          (val2 (value-of exp2 env)))
      (let ((num1 (expval->num val1))
            (num2 (expval->num val2)))
        (num-val (- num1 num2))))))

(define zero?-op
  (lambda (exp1 env)
    (let ((val1 (value-of exp1 env)))
      (let ((num1 (expval->num val1)))
        (if (zero? num1)
            (bool-val #t)
            (bool-val #f))))))

(define if-op
  (lambda (exp1 exp2 exp3 env)
    (let ((val1 (value-of exp1 env)))
      (if (expval->bool val1)
          (value-of exp2 env)
          (value-of exp3 env)))))

(define let-op
  (lambda (var exp1 body env)
    (let ((val1 (value-of exp1 env)))
      (value-of body
                (extend-env var val1 env)))))

(define minus-op
  (lambda (exp1 env)
    (value-of (diff-exp (const-exp 0) exp1)
              env)))

(define add-op
  (lambda (exp1 exp2 env)
    (num-val (+ (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))

(define mul-op
  (lambda (exp1 exp2 env)
    (num-val (* (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))

(define div-op
  (lambda (exp1 exp2 env)
    (let ((val2 (expval->num (value-of exp2 env))))
      (if (= 0 val2)
          (report-division-by-zero)
          (num-val (/ (expval->num (value-of exp1 env))
                      val2))))))

(define equal?-op
  (lambda (exp1 exp2 env)
    (bool-val (= (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))))

(define greater?-op
  (lambda (exp1 exp2 env)
    (bool-val (> (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))))

(define less?-op
  (lambda (exp1 exp2 env)
    (bool-val (< (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))))

(define cons-op
  (lambda (exp1 exp2 env)
    (list-val (value-of exp1 env)
              (value-of exp2 env))))

(define car-op
  (lambda (exp1)
    (car (expval->list exp1))))

(define cdr-op
  (lambda (exp1)
    (cdr (expval->list exp1))))

(define null?-op
  (lambda (exp1 env)
    (let ((val1 (value-of exp1 env)))
      (cases expval val1
        (emptylist-val () (bool-val #t))
        (else (bool-val #f))))))

(define emptylist-op
  (lambda ()
    (emptylist-val)))

(define list-op
  (lambda (exps env)
    (list->expval (map (lambda (expr) (value-of expr env))
                       exps))))

;; 3.12
;; expressions
(cond-exp
 (clauses (list-of (list-of expression?))))

;; value-of
;; writing this in the style of 3.11 is better
(cond-exp (clauses) (cond-op clauses env))

;; Listof(List(Expression, Expression)) -> ExpVal
(define cond-op
  (lambda (clauses env)
    (if (null? clauses)
        (error-all-false-predicates)
        (let* ((clause (car clauses))
               (predicate (car clause))
               (val (value-of predicate env)))
          (if (expval->bool val)
              (let ((consequent (cadr clause))) ; just to give a name
                (value-of consequent env))
              (cond-op (cdr clauses) env))))))

;; 3.13
(define-datatype expval expval?
  (num-val
   (num number?)))

;; value-of
(zero?-exp (exp1)
           (let ((val1 (value-of exp1 env)))
             (let ((num1 (expval->num val1)))
               (if (zero? num1)
                   (num-val 1)
                   (num-val 0)))))

(if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
          (if (zero? (expval->num val1))
              (value-of exp3 env)
              (value-of exp2 env))))

;; 3.14
;; Bool-exp ::= zero? (Expression)
;;          ::= equal? (Expression, Expression)
;;          ::= greater? (Expression, Expression)
;;          ::= less? (Expression, Expression)
(define-datatype bool-exp bool-exp?
  (zero?-exp
   (exp expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?)))

;; Bool-exp -> ExpVal
(define value-of-bool-exp
  (lambda (exp env)
    (cases bool-exp exp
      (zero?-exp (exp)
                 (let ((val (value-of exp env)))
                   (let ((num (expval->num val)))
                     (if (zero? num)
                         (bool-val #t)
                         (bool-val #f)))))
      (equal?-exp (exp1 exp2)
                  (bool-val (= (expval->num (value-of exp1 env))
                               (expval->num (value-of exp2 env)))))
      (greater?-exp (exp1 exp2)
                    (bool-val (> (expval->num (value-of exp1 env))
                                 (expval->num (value-of exp2 env)))))
      (less?-exp (exp1 exp2)
                 (bool-val (< (expval->num (value-of exp1 env))
                              (expval->num (value-of exp2 env))))))))

;; expressions
(if-exp
 (bool-exp1 bool-exp?)
 (exp2 expression?)
 (exp3 expression?))
(bool-expr
 (bool-exp1 bool-exp?))

;; value-of
(if-exp (bool-exp1 exp2 exp3)
        (let ((val1 (value-of-bool-exp bool-exp1 env)))
          (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))
(bool-expr (bool-exp1)
           (value-of-bool-exp bool-exp1 env))

;; 3.15
;; expressions
(print-exp
 (exp expression?))

;; value-of
(print-exp (exp)
           (display (value-of exp env))
           (num-val 1))

;; this operation has a side effect

;; 3.16
;; expressions
(let-exp
 (vars (list-of identifier?))
 (exps (list-of expression?))
 (body expression?))

;; value-of
(let-exp (vars exps body)
         (value-of body
                   (extend-env* vars
                                (map (lambda (exp)
                                       (value-of exp env))
                                     exps)
                                env)))

;; 3.17
;; expressions
(let*-exp
 (vars (list-of identifier?))
 (exps (list-of expression?))
 (body expression?))

;; value-of
(let*-exp (vars exps body)
          (let*-op vars exps body env))

(define let*-op
  (lambda (vars exps body env)
    (if (null? vars)
        (value-of body env)
        (let*-op (cdr vars)
                 (cdr exps)
                 body
                 (extend-env (car vars)
                             (value-of (car exps) env)
                             env)))))

;; 3.18
;; expressions
(unpack-exp
 (vars (list-of identifier?))
 (exp expression?)
 (body expression?))

;; value-of
(unpack-exp (vars exp body)
            (unpack-op vars
                       (expand-expval->list (value-of exp env))
                       body
                       env))

(define unpack-op
  (lambda (vars lst body env)
    (if (null? vars)
        (if (null? lst)
            (value-of body env)
            (report-too-many-elements lst))
        (unpack-op (cdr vars)
                   (cdr lst)
                   body
                   (extend-env (car vars)
                               (car lst)
                               env)))))

(define expand-expval->list
  (lambda (val)
    (cases expval val
      (emptylist-val () '())
      (list-val (first rest)
                (cons first (expand-expval->list rest)))
      (else (report-not-a-list val)))))