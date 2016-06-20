;;; Exercise 1.33
(define mark-leaves-with-red-depth
  (lambda (btree)
	()))

(mark-leaves-with-red-depth
 (interior-node
  'red
  (interior-node
   'bar
   (leaf 26)
   (leaf 12))
  (interior-node
   'red
   (leaf 11)
   (interior-node
	'quux
	(leaf 117)
	(leaf 14)))))
