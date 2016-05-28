;;; Exercise 1.23
(define list-index
  (lambda (pred lst)
	(cond ((null? lst) #f)
		  (else
		   (if (pred (car lst))
			   0
			   (+ 1 (list-index pred (cdr lst))))))))
