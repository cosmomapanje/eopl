;;; Exercise 1.20
(define count-occurrences
  (lambda (a lst)
	(cond ((null? lst) 0)
		  ((atom? (car lst))
		   (if (eqv? a (car lst))
			   (+ 1 (count-occurrences a (cdr lst)))
			   (+ 0 (count-occurrences a (cdr lst)))))
		  (else
		   (+ (count-occurrences a (car lst))
			  (count-occurrences a (cdr lst)))))))
