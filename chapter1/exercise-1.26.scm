;;; Exercise 1.26
(define up
  (lambda (lst)
    (cond ((null? lst) '())
          (else
           (if (list? (car lst))
               (append (car lst) (up (cdr lst)))
               (cons (car lst) (up (cdr lst))))))))

(up '((1 2) (3 4)))
(up '((x (y)) z))
