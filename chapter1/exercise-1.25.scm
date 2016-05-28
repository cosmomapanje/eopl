;;; Exercise 1.25
(define exists?
  (lambda (pred lst)
    (cond ((null? lst) #f)
          (else
           (if (pred (car lst))
               #t
               (exists? pred (cdr lst)))))))
