;;; Exercise 2.26
(define-datatype Red-blue-tree Red-blue-tree?
  (Red-blue-subtree Red-blue-subtree?))

(define-datatype Red-blue-subtree Red-blue-subtree?
  (red-node
   (left Red-blue-subtree?)
   (right Red-blue-subtree?))
  (blue-node
   (subtrees (list-of Red-blue-subtree?)))
  (leaf-node number?))
