#lang eopl

(define informative-report-list-too-short
  (lambda (lst n)
    (eopl:error 'nth-element
                "~s does not have ~s elements.~%"
                lst
                (+ n 1))))

(define nth-element
  (lambda (lst n)
    (define iter
      (lambda (l m)
        (if (null? l)
            (informative-report-list-too-short lst n)
            (if (zero? m)
                (car l)
                (iter (cdr l) (- m 1))))))
    (iter lst n)))

(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

;; 1.12

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons (if (symbol? (car slist))
                  (if (eqv? (car slist) old)
                      new
                      (car slist))
                  (subst new old (car slist)))
              (subst new old (cdr slist))))))

;; 1.13
(define subst
  (lambda (new old slist)
    (map (lambda (sexp)
           (subst-in-s-exp new old sexp))
         slist)))

(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))