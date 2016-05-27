;;; Exercise 1.18
(define swapper
  (lambda (s1 s2 slist)
	(cond ((null? slist) '())
		  ((symbol? (car slist))
		   (if (eqv? s1 (car slist))
			   (cons s2 (swapper s1 s2 (cdr slist)))
			   (if (eqv? s2 (car slist))
				   (cons s1 (swapper s1 s2 (cdr slist)))
				   (cons (car slist) (swapper s1 s2 (cdr slist))))))
		  (else
		   (cons (swapper s1 s2 (car slist))
				 (swapper s1 s2 (cdr slist)))))))
