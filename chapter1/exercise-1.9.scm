;;; Exercise 1.9
(define remove
  (lambda (s los)
    (cond ((null? los) '())
	  ((list? (car los))
	   (cons (remove s (car los))
		 (remove s (cdr los))))
	  (else
	   (if (eqv? s (car los))
	       (remove s (cdr los))
	       (cons (car los) (remove s (cdr los))))))))
