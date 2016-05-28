;;; Exercise 1.34
(define path
  (lambda (num bst)
    (cond ((< num (car bst))
           (cons 'left (path num (car (cdr bst)))))
          ((> num (car bst))
           (cons 'right (path num (car (cdr (cdr bst))))))
          ((eqv? num (car bst))
           '()))))
