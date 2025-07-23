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