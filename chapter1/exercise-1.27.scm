;;; Exercise 1.27
(define flatten
  (lambda (lst)
    (cond ((null? lst) '())
          (else
           (if (list? (car lst))
               (append (flatten (car lst)) (flatten (cdr lst)))
               (cons (car lst) (flatten (cdr lst))))))))

(flatten '(a b c))
(flatten '((a) () (b ()) () (c)))
(flatten '((a b) c (((d)) e)))
(flatten '(a b (() (c))))
