;; 2.2.1

(define nth-elt
  (lambda (lst n)
    (if (null? lst)
	(error "nth-elt: list too short")
	(if (pair? lst)
	    (if (zero? n)
		(car lst)
		(nth-elt (cdr lst) (- n 1)))
	    (error "nth-elt: not a list")))))

(define list-length
  (lambda (lst)
    (if (null? lst)
	0
	(if (pair? lst)
	    (+ 1 (list-length (cdr lst)))
	    (error "list-length: not a list")))))

;; 2.2.5

(define subst
  (lambda (new old slst)
    (map (lambda (sexp) (subst-sexp new old sexp)) slst)))

(define subst-sexp
  (lambda (new old sexp)
    (if (symbol? sexp)
	(if (eq? old sexp) new sexp)
	(subst new old sexp))))
	    
;;; 2.2.7

;; 1
(define duple
  (lambda (n x)
    (if (zero? n)
	'()
	(cons x (duple (- n 1) x)))))

;; 2
(define invert
  (lambda (lst)
    (if (null? lst)
	'()
	(cons (list (cadar lst) (caar lst))
	      (invert (cdr lst))))))

;; 3
(define list-index
  (lambda (s los)
    (li s los 0)))

(define li
  (lambda (s los index)
    (if (null? los)
	-1
	(if (eq? s (car los))
	    index
	    (li s (cdr los) (+ index 1))))))

;; 4
(define vector-index
  (lambda (s vos)
    (vi s vos (vector-length vos))))

(define vi
  (lambda (s vos index)
    (if (zero? index)
	-1
	(if (eq? s (vector-ref vos (- index 1)))
	    (- index 1)
	    (vi s vos (- index 1))))))

;; 5
(define ribassoc
  (lambda (s los v fail-value)
    (ribassoc-iter s los v fail-value 0)))

(define ribassoc-iter
  (lambda (s los v fail-value count)
    (if (null? los)
	fail-value
	(if (eq? s (car los))
	    (vector-ref v count)
	    (ribassoc-iter
	     s (cdr los) v fail-value (+ count 1))))))

;; 6
(define filter-in
  (lambda (p lst)
    (if (null? lst)
	'()
	(if (p (car lst))
	    (cons (car lst) (filter-in p (cdr lst)))
	    (filter-in p (cdr lst))))))

;; 7
(define product
  (lambda (los1 los2)
    (if (null? los1)
	'()
	(append (prdct (car los1) los2)
		(product (cdr los1) los2)))))

(define prdct
  (lambda (s los)
    (if (null? los)
	'()
	(cons (list s (car los)) (prdct s (cdr los))))))

;; 8
(define swapper
  (lambda (s1 s2 slst)
    (if (null? slst)
	'()
	(cons (swap-in-sexp s1 s2 (car slst))
	      (swapper s1 s2 (cdr slst))))))

(define swap-in-sexp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
	(if (eq? s1 sexp)
	    s2
	    (if (eq? s2 sexp)
		s1
		sexp))
	(swapper s1 s2 sexp))))

;; 9
(define rotate
  (lambda (los)
    (if (null? los)
	'()
	(cons (last los) (but-last los)))))

(define last
  (lambda (los)
    (if (null? los)
	'()
	(if (null? (cdr los))
	    (car los)
	    (last (cdr los))))))

(define but-last
  (lambda (los)
    (if (null? los)
	'()
	(if (null? (cdr los))
	    '()
	    (cons (car los) (but-last (cdr los)))))))

;; 2.2.8

;; 1
(define down
  (lambda (lst)
    (map list lst)))

;; 2
(define up
  (lambda (lst)
    (if (null? lst)
	'()
	(if (symbol? (car lst))
	    (cons (car lst) (up (cdr lst)))
	    (append (car lst) (up (cdr lst)))))))

;; 3
(define count-occurrences
  (lambda (s slst)
    (if (null? slst)
	0
	(+ (count-in-sexp s (car slst))
	   (count-occurrences s (cdr slst))))))

(define count-in-sexp
  (lambda (s sexp)
    (if (symbol? sexp)
	(if (eq? s sexp) 1 0)
	(count-occurrences s sexp))))

;; 4
(define flatten
  (lambda (slst)
    (if (null? slst)
	'()
	(append (flatten-sexp (car slst))
		(flatten (cdr slst))))))

(define flatten-sexp
  (lambda (sexp)
    (if (symbol? sexp)
	(list sexp)
	(flatten sexp))))

;; 5
(define merge
  (lambda (lon1 lon2)
    (if (null? lon1)
	lon2
	(if (null? lon2)
	    lon1
	    (if (<= (car lon1) (car lon2))
		(cons (car lon1) (merge (cdr lon1) lon2))
		(cons (car lon2) (merge lon1 (cdr lon2))))))))

;; 2.2.9

;; 1
(define path
  (lambda (n bst)
    (if (null? bst)
	'()
	(if (= n (car bst))
	    '()
	    (if (< n (car bst))
		(cons 'l (path n (cadr bst)))
		(cons 'r (path n (caddr bst))))))))

;; 2. beyond present understanding. requires early exit on trees.

;; 3. same

;; 4
(define compose
  (lambda procs
    (comp procs)))

(define comp
  (lambda (procs)
    (if (null? procs)
	(lambda (x) x)
	(if (null? (cdr procs))
	    (car procs)
	    (binary-compose (car procs) (comp (cdr procs)))))))

(define binary-compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; 5
(define sort
  (lambda (lon)
    (if (null? lon)
	'()
	(cons (lowest lon)
	      (sort (remove-first-occurrence
		     (lowest lon)
		     lon))))))

(define lowest
  (lambda (lon)
    (lowest-iter (car lon) lon)))

(define lowest-iter
  (lambda (n lon)
    (if (null? lon)
	n
	(if (< (car lon) n)
	    (lowest-iter (car lon) (cdr lon))
	    (lowest-iter n (cdr lon))))))

(define remove-first-occurrence
  (lambda (n lon)
    (if (null? lon)
	'()
	(if (= n (car lon))
	    (cdr lon)
	    (cons (car lon)
		  (remove-first-occurrence n (cdr lon)))))))

;; 6
(define sort
  (lambda (p lon)
    (if (null? lon)
	'()
	(cons (first p lon)
	      (sort p
		    (remove-first-occurrence
		     (first p lon)
		     lon))))))

(define first
  (lambda (p lon)
    (first-iter p (car lon) lon)))

(define first-iter
  (lambda (p n lon)
    (if (null? lon)
	n
	(if (p (car lon) n)
	    (first-iter p (car lon) (cdr lon))
	    (first-iter p n (cdr lon))))))

(define remove-first-occurrence
  (lambda (n lon)
    (if (null? lon)
	'()
	(if (= (car lon) n)
	    (cdr lon)
	    (cons (car lon)
		  (remove-first-occurrence n (cdr lon)))))))

;; 2.3.1

;; there is no bound reference to x in the body, hence x does not occur bound in this exp.
;; but there is no free reference to x in the body, hence x does not occur free in this exp.
;(free-vars '(lambda (x) y)) ; (y)

;; a formal parameter is not a variable reference. therefore it does not occur free nor bound.
;(free-vars '(lambda (x) (lambda (x) x)))  ; ()

;; how do we know y is bound
;; there exists a formal parameter y and y occurs free in (lambda (x) y)
;; how do we know y occurs free in (lambda (x) y)
;; this expression is of the form (lambda (y) E') where y is in fact different from y and y occurs free in E'
;; E' is a variable reference and E' is the same as y
;(free-vars '(lambda (y) (lambda (x) y))) ; ()

;; there is a free reference and a bound reference to x. relative to the inner expression, x occurs bound. relative to the operand exp there is a free reference to x. since at least one free reference to x exists, then x occurs free somewhere in this entire expression.
;(free-vars '((lambda (x) x) x)) ; (x)

;(free-vars '(lambda (f) (lambda (x) (f x)))) ; ()

;(free-vars '((lambda (a) (lambda (b) ((a b) c)))
;	     (lambda (x) (lambda (y) ((x y) z))))) ; (c z)

;(free-vars '((lambda (a) (a c))
;	     (lambda (x) (x c)))) ; (c)

;(free-vars '(lambda (a) (lambda (b) (lambda (c) (lambda (d) (e (f (g a)))))))) ; (e f g)

;; starting with no context, we have no formal parameters to track. we also must remove duplicates. we do not capture whether two distinct variables with the same name occur free.
(define free-vars
  (lambda (exp)
    (no-duplicates (all-free exp '()))))

;; varref? if the symbol exp is in the list of known formal parameters then it is not free
;; lambda? find all-free in the body with the formal parameter added to the list of known formal params
;; application? find all-free in the operator and operand.
(define all-free
  (lambda (exp formal-parameters)
    (if (varref? exp)
	(if (member exp formal-parameters)
	    '()
	    (list exp))
	(if (lambda-exp? exp)
	    (all-free (body exp)
		      (cons (formal-parameter exp)
			    formal-parameters))
	    (append (all-free (operator exp) formal-parameters)
		    (all-free (operand exp) formal-parameters))))))

(define varref? symbol?)
(define lambda-exp?
  (lambda (exp)
    (eq? (car exp) 'lambda))) ; assuming every exp has no syntax other than that of the grammar
(define application?
  (lambda (exp)
    (= (length exp) 2)))
(define body caddr)

(define formal-parameter
  (lambda (exp)
    (car (cadr exp))))
(define operator car)
(define operand cadr)

(define no-duplicates
  (lambda (lst)
    (if (null? lst)
	'()
	(if (member (car lst) (cdr lst))
	    (no-duplicates (cdr lst))
	    (cons (car lst) (no-duplicates (cdr lst)))))))

;; with no context given, find all-bound in the exp with no current formal parameters
(define bound-vars
  (lambda (exp)
    (no-duplicates (all-bound exp '()))))

;; varref? no such bound variables in a varref production
;; lambda? if the formal parameter is found to be free in the body, then we cons that symbol (because it is by definition a bound variable) to the set of bound variables in the body of the exp, with that symbol also being added to the list of known formal parameters. if the formal parameter is not found to be free in the body, then we find all-bound in the body with this formal parameter added to the list of known formal parameters
;; application: append all-bound from the operator and operand.
(define all-bound
  (lambda (exp formal-parameters)
    (if (varref? exp)
	'()
	(if (lambda-exp? exp)
	    (if (member (formal-parameter exp)
			(free-vars (body exp)))
		(cons (formal-parameter exp)
		      (all-bound (body exp)
				 (cons (formal-parameter exp)
				       formal-parameters)))
		(all-bound (body exp)
			   (cons (formal-parameter exp)
				 formal-parameters)))
	    (append (all-bound (operator exp) formal-parameters)
		    (all-bound (operand exp) formal-parameters))))))
	    
;(bound-vars '(lambda (x) y)) ; ()

;(bound-vars '(lambda (x) (lambda (x) x)))  ; (x)

;(bound-vars '(lambda (y) (lambda (x) y))) ; (y)

;; there exists at least one bound reference to x somewhere in this expression. therefore x occurs bound in the whole expression.
;(bound-vars '((lambda (x) x) x)) ; (x)

;(bound-vars '(lambda (f) (lambda (x) (f x)))) ; (f x)

;(bound-vars '((lambda (a) (lambda (b) ((a b) c)))
;	     (lambda (x) (lambda (y) ((x y) z))))) ; (a b x y)

;(bound-vars '((lambda (a) (a c))
;	     (lambda (x) (x c)))) ; (a x)

;(bound-vars '(lambda (a) (lambda (b) (lambda (c) (lambda (d) (e (f (g a)))))))) ; (a)

;; 2.3.2
(define free?
  (lambda (var exp)
    (if (varref? exp)
	(eq? exp var)
	(if (lambda-exp? exp)
	    (if (eq? var (formal-parameter exp))
	        #f ; if var occurs free or bound in the body, then var does not occur free here. if there are no variable references to var, then it is false regardless, whether or not there are formal parameters named var.
		(free? var (body exp))) ; otherwise find if var is free in the body
	    (or (free? var (operator exp)) ; otherwise find if var is free in operator or operand
		(free? var (operand exp)))))))

(free? 'y '(lambda (x) y)) ; #t

(free? 'x '(lambda (x) y)) ; #f

(free? 'z '(lambda (x) y)) ; #f

(free? 'x '(lambda (x) (lambda (x) x)))  ; #f

(free? 'y '(lambda (y) (lambda (x) y))) ; #f

(free? 'x '((lambda (x) x) x)) ; #t

(free? 'f '(lambda (f) (lambda (x) (f x)))) ; #f

(free? 'z '((lambda (a) (lambda (b) ((a b) c)))
	    (lambda (x) (lambda (y) ((x y) z))))) ; #t

(free? 'c '((lambda (a) (a c))
	    (lambda (x) (x c)))) ; #t

(free? 'f '(lambda (a) (lambda (b) (lambda (c) (lambda (d) (e (f (g a)))))))) ; #t

(define bound?
  (lambda (var exp)
    (if (varref? exp)
	#f
	(if (lambda-exp? exp)
	    (if (eq? var (formal-parameter exp)) ; true or not, we check if var is bound in the body
	        (or (free? var (body exp))
		    (bound? var (body exp)))
		(bound? var (body exp)))
	    (or (bound? var (operator exp))
		(bound? var (operand exp))))))) ; application


(bound? 'y '(lambda (x) y)) ; #f

(bound? 'x '(lambda (x) y)) ; #f

(bound? 'z '(lambda (x) y)) ; #f

(bound? 'x '(lambda (x) (lambda (x) x)))  ; #t

(bound? 'y '(lambda (y) (lambda (x) y))) ; #t

(bound? 'x '((lambda (x) x) x)) ; #t

(bound? 'f '(lambda (f) (lambda (x) (f x)))) ; #t

(bound? 'z '((lambda (a) (lambda (b) ((a b) c)))
	    (lambda (x) (lambda (y) ((x y) z))))) ; #f

(bound? 'c '((lambda (a) (a c))
	    (lambda (x) (x c)))) ; #f

(bound? 'f '(lambda (a) (lambda (b) (lambda (c) (lambda (d) (e (f (g a)))))))) ; #f

;; 2.3.3
'((lambda (x) x) x)

;; 2.3.4
;; x occurs free and the value of the lambda calculus expression is independent of x. the naming of the formal parameter in this case must be anything but y.
'((lambda (x) y) x)

;; 2.3.5
;; redefine free-vars
;; LcExp -> Listof(Var)
(define free-vars
  (lambda (exp)
    (no-duplicates (all-free exp '()))))

;; LcExp * Listof(Var) -> Listof(Var)
(define all-free
  (lambda (exp params)
    (if (varref? exp)
	(if (member exp params)
	    '()
	    (list exp))
	(if (lambda-exp? exp)
	    (all-free (body exp)
		      (append (formal-parameters exp)
			      params))
	    (append (all-free (operator exp) params)
		    (reduce append '()
			    (map (lambda (operand)
				   (all-free operand params)) ; Listof(Listof(Var))
				 (operands exp))))))))

(define operands cdr)
(define formal-parameters
  (lambda (exp)
    (if (null? (cadr exp)) ; no formal parameters
	'()
	(if (null? (cdadr exp)) ; only one formal parameter
	    (list (caadr exp))
	    (cadr exp))))) ; at least one formal parameter

(free-vars '(lambda () y)) ; (y)
(free-vars '(lambda (a b) y)) ; (y)
(free-vars '(lambda (x y z) y)) ; ()
(free-vars '((lambda () (x y)) (lambda (y x) y))) ; (x y)
(free-vars '(lambda (a b c) (lambda (d e f) (g (b e))))) ; (g)
(free-vars '(a b (lambda (x) (c x)) d)) ; (a b c d)

(define bound-vars
  (lambda (exp)
    (no-duplicates (all-bound exp '()))))

;; LcExp * Listof(Var) -> Listof(Var)
(define all-bound
  (lambda (exp params)
    (if (varref? exp)
	'()
	(if (lambda-exp? exp)
	    (append (occurrences (formal-parameters exp)
				 (free-vars (body exp)))
		    (all-bound (body exp)
			       (append (formal-parameters exp)
				       params)))
	    (append (all-bound (operator exp) params)
		    (reduce append '()
			    (map (lambda (operand)
				   (all-bound operand params))
				 (operands exp))))))))

;; usage: makes a list of all the elements in members that appear in lst at least once
(define occurrences
  (lambda (members lst)
    (if (null? members)
	'()
	(if (member (car members) lst)
	    (cons (car members) (occurrences (cdr members) lst))
	    (occurrences (cdr members) lst)))))

(bound-vars '(lambda () y)) ; ()
(bound-vars '(lambda (a b) y)) ; ()
(bound-vars '(lambda (x y z) y a b)) ; (y)
(bound-vars '(a (lambda (y x) y) g)) ; (y)
(bound-vars '((lambda () (x y)) (lambda (y x) y) g)) ; (y)
(bound-vars '(lambda (a b c) (lambda (d e f) (g (b e))))) ; (b e)
(bound-vars '((lambda (a) a) (lambda (b) b) (lambda (c) c) d)) ; (a b c)

;; the exercise said to modify the definitions of "occurs free" and "occurs bound". we dont have procedures named those. they really mean to modify the definitions in the book. in this exercise, the informal definition.

;; A variable occurs free in an expression if there exists a free reference to that variable in that expression. A variable occurs bound in an expression if there exists a bound reference to that variable in that expression. A variable reference is said to be bound in an expression if it refers to a formal parameter introduced in the expression.

;; this definition is virtually no different.

;; 2.3.6
;; <exp> ::= (if <exp> <exp> <exp>)
;; A variable x occurs free in an expression E if and only if
;; E is of the form (if E_1 E_2 E_3) and x occurs free in E_1 or E_2 or E_3.

;; A variable x occurs bound in an expression E if and only if
;; E is of the (if E_1 E_2 E_3) and x occurs bound in E_1 or E_2 or E_3.

;; 2.3.7
;; a quoted variable is no longer a variable reference. hence quoted variables do not occur free or occur bound. thus it makes no difference to the set of free and bound variables.

;; 2.3.10

(define if-exp?
  (lambda (exp)
    (eq? (car exp) 'if)))

(define predicate cadr)
(define consequent caddr)
(define alternative cadddr)

(define all-free
  (lambda (exp params)
    (if (varref? exp)
	(if (memq exp params)
	    '()
	    (list exp))
	(if (if-exp? exp)
	    (append (all-free (predicate exp) params)
		    (all-free (consequent exp) params)
		    (all-free (alternative exp) params))
	    (if (lambda-exp? exp)
		(all-free (body exp)
			  (append (formal-parameters exp)
				  params))
		(append (all-free (operator exp) params)
			(reduce append
				'()
				(map (lambda (l)
				       (all-free l params))
				     (operands exp)))))))))

;; LexLcExp ::= (<varref> : <num> <num>)
;;            | (if LexLcExp LexLcExp LexLcExp)
;;            | (lambda ({Var}*) LexLcExp)
;;            | ({LexLcExp}+)

;; LcExp -> LexLcExp
(define lexical-address
  (lambda (exp)
    (transform-exp exp '() (free-vars exp))))

;; LcExp * Env * Listof(Var) -> LexLcExp
(define transform-exp
  (lambda (exp env free)
    (if (varref? exp)
	(list exp ': (depth exp env) (position exp env free))
	(if (if-exp? exp)
	    (list 'if
		  (transform-exp (predicate exp) env free)
		  (transform-exp (consequent exp) env free)
		  (transform-exp (alternative exp) env free))
	    (if (lambda-exp? exp)
		(list 'lambda
		      (formal-parameters exp)
		      (transform-exp
		       (body exp)
		       (extend (formal-parameters exp)  env)
		       free))
	        (cons (transform-exp (operator exp) env free)
		      (map (lambda (operand)
			     (transform-exp operand env free))
			   (operands exp))))))))

;; Env = Listof(Listof(Var))
;; Listof(Var) * Env -> Env
(define extend
  (lambda (params env)
    (if (null? env)
	(list params)
	(cons params env))))

;; ((v v ... v) (v v ... v) ... (v v ... v))

(define depth
  (lambda (var env)
    (depth-iter 0 var env)))

(define depth-iter
  (lambda (d var env)
    (if (null? env)
        d ; must be a free variable outside of our context. we dont assume unbound here
	(if (member var (car env))
	    d
	    (depth-iter (+ 1 d) var (cdr env))))))

(define position
  (lambda (var env free)
    (if (null? env)
        (place var free)
	((lambda (p)
	   (if (eq? p 'not-here)
	       (position var (cdr env) free)
	       p))
	 (place var (car env))))))

(define place
  (lambda (var los)
    (place-iter 0 var los)))

(define place-iter
  (lambda (n var los)
    (if (null? los)
	'not-here
	(if (eq? var (car los))
	    n
	    (place-iter (+ n 1) var (cdr los))))))

(lexical-address
 '(lambda () a))

(lexical-address
 '(a b (lambda (x) (x y))))

;; want
((a : 0 0) (b : 0 1) (lambda (x) ((x : 0 0) (y : 1 2))))
;; gets
((a : 0 0) (b : 0 2) (lambda (x) ((x : 0 0) (y : 1 1))))
;; the lexaddrs substituted in are still consistent, so this result is fine

(lexical-address
 '(lambda (a b c)
    (if (eq? b c)
	((lambda (c)
	   (cons a c))
	 a)
	b)))

(lambda (a b c)
  (if ((eq? : 1 0) (b : 0 1) (c : 0 2))
      ((lambda (c)
	 ((cons : 2 1) (a : 1 0) (c : 0 0)))
       (a : 0 0))
      (b : 0 1)))

(lexical-address
 '(lambda (a b c)
    ((lambda (x y) (cons x (cons y z)))
     (lambda (a x) (skibidi x a))
     (if (a x)
	 d
	 (lambda () (a toilet))))))

;; want
(lambda (a b c)
  ((lambda (x y)
     ((cons : 2 0) (x : 0 0) ((cons : 2 0) (y : 0 1) (z : 2 1))))
   (lambda (a x)
     ((skibidi : 2 5) (x : 0 1) (a : 0 0)))
   (if ((a : 0 0) (x : 1 2))
       (d : 1 3)
       (lambda () ((a : 1 0) (toilet: 2 4))))))

;; it happens that the env is constructed as such: cons z x d toilet skibidi
(lambda (a b c)
  ((lambda (x y)
     ((cons : 2 0) (x : 0 0) ((cons : 2 0) (y : 0 1) (z : 2 1))))
   (lambda (a x)
     ((skibidi : 2 5) (x : 0 1) (a : 0 0)))
   (if ((a : 0 0) (x : 1 2))
       (d : 1 3)
       (lambda () ((a : 1 0) (toilet : 2 4))))))


;; 2.3.11
;; the variable reference to a refers to the nearest declaration of a, and thus the lexical address should be written (a : 0 0)

;; 2.3.12
(lambda (x)
  (lambda (y)
    x))

;; 2.3.13

;; does not handle free variables
(define un-lexical-address
  (lambda (exp)
    (transform-exp exp '())))

;; LexLcExp * Env -> LcExp
(define transform-exp
  (lambda (exp env)
    (if (varref? exp)
	(substitute exp env)
	(if (if-exp? exp)
	    (let ((p-result (transform-exp (predicate exp) env))
		  (c-result (transform-exp (consequent exp) env))
		  (a-result (transform-exp (alternative exp) env)))
	      (if (not (or p-result c-result a-result)) ; at least one false
		  #f ; then we return #f with nothing on the stack to hinder this
		  (cons 'if (append p-result c-result a-result)))) ;otherwise take appropriate result
	    (if (lambda-exp? exp)
		(let ((result
		       (transform-exp
			(body exp)
			(extend (formal-parameters exp) env))))
		  (if result
		      (list 'lambda
			    (formal-parameters exp)
			    result)
		      #f))
		(let ((result (map (lambda (l)
				     (transform-exp l env))
				   (extend (operator exp)
					   (operands exp)))))
		  (if (member #f result) ; any failed to substitute?
		      #f
		      result)))))))

;; Lexaddr = (: Num Num)
(define varref?
  (lambda (exp)
    (eq? (car exp) ':)))

;; Lexaddr * Env -> Maybe(Var,False)
(define substitute
  (lambda (lexaddr env)
    (if (null? env)
	#f ; free var
	(let ((frame
	       (frame-at-index
		env
		(lexaddr-depth lexaddr))))
	  (if frame
	      (let ((var
		     (var-at-index
		      frame
		      (lexaddr-position lexaddr))))
		(if var
		    (if (correct-scope?
			 (get-lexaddr var env) lexaddr)
			var
			#f)
		    #f))
	      #f)))))

(define var-at-index safe-list-ref)
(define frame-at-index safe-list-ref)
(define safe-list-ref
  (lambda (l n)
    (if (null? l)
	#f
	(if (= 0 n)
	    (car l)
	    (safe-list-ref (cdr l) (- n 1))))))

;; no free variables
(define position
  (lambda (sym list-of-los)
    (if (null? list-of-los)
	#f
	(let ((n (place sym (car list-of-los))))
	  (if (eq? n 'not-here)
	      (position sym (cdr list-of-los))
	      n)))))

(define get-lexaddr
  (lambda (var env)
    (list ': (depth var env) (position var env))))

(define lexaddr-depth cadr)
(define lexaddr-position caddr)

(define correct-scope? lexaddr=?)
(define lexaddr=?
  (lambda (a b)
    (if (= (lexaddr-depth a) (lexaddr-depth b))
        (= (lexaddr-position a) (lexaddr-position b))
	#f)))
	
(un-lexical-address '(lambda (a) (lambda (b c) ((: 1 0) (: 0 0) (: 0 1)))))
;; (lambda (a) (lambda (b c) (a b c)))
(un-lexical-address '(lambda (a) (lambda (a) (: 1 0)))) ;#f
(un-lexical-address '(lambda (a) (lambda (b) (lambda (c) ((: 0 0) (: 1 0) (: 2 0))))))
;; (lambda (a) (lambda (b) (lambda (c) (c b a))))
(un-lexical-address '(lambda (a b) (lambda () ((: 1 1) (: 1 0)))))
;; (lambda (a b) (lambda () (b a)))
(un-lexical-address '(lambda () (lambda (a) (lambda () (: 1 0)))))
;; (lambda () (lambda (a) (lambda () a)))
(un-lexical-address '(: 1 0)) ; #f
(un-lexical-address '(lambda (a) (: 1 0))) ; #f
(un-lexical-address '((lambda (a b) ((: 0 1) (: 0 0))) (lambda (a b) ((: 0 1) (: 1 0))))) ; #f
(un-lexical-address '(lambda (c) ((lambda (a b) ((: 0 1) (: 0 0))) (lambda (a b) ((: 0 1) (: 1 0))))))
;; (lambda (c) ((lambda (a b) (b a)) (lambda (a b) (b c))))

;; 2.3.14 (extends 2.3.2) [y/x] = [x <- y] = [var1/var2] = [var2 <- var1]
;; rewrite [y/x] as [x <- y], the right argument maps to the left argument
;; usage: exp[var1 <- var2]
(define rename 
  (lambda (exp var1 var2)
    (if (free? var1 exp)
	#f ; easy way to exit early with #f
	(rename-i exp var1 var2))))

(define rename-i
  (lambda (exp var1 var2)
    (if (varref? exp)
	(if (eq? exp var2) var1 exp)
	(if (lambda-exp? exp)
	    (if (eq? (formal-parameter exp) var1)
	        exp ; var1 cannot occur free in the body. hence we do not rename
		(if (eq? (formal-parameter exp) var2)
		    exp ; by definition of exp[var1/var2], we substitute only the free occur of var2. all references to var2 in the body expression are clearly bound
		    (list 'lambda
			  (list (formal-parameter exp))
			  (rename-i (body exp) var1 var2))))
	    (list (rename-i (operator exp) var1 var2)
		  (rename-i (operand exp) var1 var2))))))

(rename '(lambda (b) (b a)) 'c 'a) ; (lambda (b) (b c))  a maps to c
(rename '((lambda (x) x) x) 'y 'x) ; ((lambda (x) x) y)  x maps to y
(rename '(a b) 'a 'b) ; #f b is a free var

;; 2.3.15 (extends 2.3.14)
;; usage: returns (lambda (v) exp[v <- var]) or #f if v occurs free in exp where exp
;; is of the form (lambda (var) exp)
(define alpha-convert
  (lambda (exp v) ;assume exp is of the form (lambda (var) exp) as is the intention of alpha-conversion
    (let ((renamed-exp
	   (rename (body exp) v (formal-parameter exp))))
      (if renamed-exp
	  (list 'lambda (list v) renamed-exp)
	  #f))))

(alpha-convert '(lambda (a) (lambda (b) (b a))) 'c)
(alpha-convert '(lambda (x) ((lambda (x) x) x)) 'y)
(alpha-convert '(lambda (x) (y x)) 'y)

