;;; Exercise 1.32
(define double-tree
  (lambda (btree)
	(cond ((null? btree) '())
		  ((list? btree)
		   (list (car btree)
				 (double-tree (car (cdr btree)))
				 (double-tree (car (cdr (cdr btree))))))
		  ((number? btree) (* 2 btree))
		  (else
		   (display 'error)))))

(double-tree '(foo 1 2))
(double-tree '(bar 1 (foo 1 2)))
(double-tree '(baz (bar 1 (foo 1 2))
                   (biz 4 5)))
