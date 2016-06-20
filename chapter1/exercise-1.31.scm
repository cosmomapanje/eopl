;;; Exercise 1.31
;;; leaf
;;; interior-node
;;; leaf?
;;; lson
;;; rson
;;; contents-of
(define leaf
  (lambda (n)
	n))

(define interior-node
  (lambda (sym l r)
	(list sym l r)))

(define leaf?
  (lambda (l)
	(number? l)))

(define lson
  (lambda (l)
	(if (leaf? l)
		'()
		(car (cdr l)))))

(define rson
  (lambda (l)
	(if (leaf? l)
		'()
		(car (cdr (cdr l))))))

(define contents-of
  (lambda (l)
	(if (leaf? l)
		l
		(car l))))
