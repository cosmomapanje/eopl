;;; Exercise 2.24

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define bintree-to-list
  (lambda (bt)
    (cases bintree bt
      (leaf-node (num)
                 (list 'leaf-node num))
      (interior-node (key left right)
                     (list 'interior-node
                           key
                           (bintree-to-list left)
                           (bintree-to-list right))))))

(define e (bintree-to-list (interior-node 'a (interior-node 'b (leaf-node 3) (leaf-node 4)) (leaf-node 5))))
