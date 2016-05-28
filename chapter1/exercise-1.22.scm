;;; Exercise 1.22
(define filter-in
  (lambda (pred lst)
	(cond ((null? lst) '())
		  (else
		   (if (pred (car lst))
			   (cons (car lst) (filter-in pred (cdr lst)))
			   (filter-in pred (cdr lst)))))))
