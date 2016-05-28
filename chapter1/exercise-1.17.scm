;;; Exercise 1.17
(define down
  (lambda (lst)
    (cond ((null? lst) '())
          (else
           (cons (cons (car lst) '())
                 (down (cdr lst)))))))
