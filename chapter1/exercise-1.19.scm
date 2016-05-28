;;; Exercise 1.19
(define list-set
  (lambda (lst n x)
	(cond ((null? lst) '())
		  ((zero? n)
		   (cons x (cdr lst)))
		  (else
		   (cons (car lst)
				 (list-set (cdr lst) (- n 1) x))))))
