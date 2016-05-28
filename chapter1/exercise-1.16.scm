;;; Exercise 1.16
(define invert
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst))
           (cons
            (cons (car (cdr (car lst))) (cons (car (car lst)) '()))
            (invert (cdr lst)))))))
