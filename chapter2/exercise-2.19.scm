;;; Exercise 2.19
;;; Definition
;;; Bintree ::= () | (Int Bintree Bintree)

(define number->bintree
  (lambda (num)
	(list num '() '())))

(define current-element
  (lambda (bintree)
	(car bintree)))

(define move-to-left-son
  (lambda (bintree)
	(car (cdr bintree))))

(define move-to-right-son
  (lambda (bintree)
	(car (cdr (cdr bintree)))))

(define at-leaf?
  (lambda (bintree)
	(null? bintree)))

(define insert-to-left
  (lambda (num bintree)
	(list (car bintree)
		  (list num
				(car (cdr bintree))
				'())
		  (car (cdr (cdr bintree))))))

(define insert-to-right
  (lambda (num bintree)
	(list (car bintree)
		  (car (cdr bintree))
		  (list num
				'()
				(car (cdr (cdr bintree)))))))

;;; Here are some test case
(number->bintree 13)
(define t1 (insert-to-right 14
							(insert-to-left 12
											(number->bintree 13))))
(move-to-left-son t1)
(current-element (move-to-left-son t1))
(at-leaf? (move-to-right-son (move-to-left-son t1)))
(insert-to-left 15 t1)
