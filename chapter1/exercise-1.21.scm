;;; Exercise 1.21
(define p
  (lambda (n sos)
    (cond ((null? sos) '())
          (else
           (cons (list n (car sos)) (p n (cdr sos)))))))

(define product
  (lambda (sos1 sos2)
    (cond ((null? sos1) '())
          (else
           (append (p (car sos1) sos2) (product (cdr sos1) sos2))))
