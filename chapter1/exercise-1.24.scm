;;; Exercise 1.24
(define every?
  (lambda (pred lst)
    (cond ((null? lst) #t)
          (else
           (if (pred (car lst))
               (every? pred (cdr lst))
               #f)))))
