;;; Exercise 1.15
(define duple
  (lambda (num exp)
	(cond ((zero? num) '())
		  (else
		   (cons exp (duple (- num 1) exp))))))
