;;; Exercise 1.23
(define list-index-with-n
  (lambda (pred lst n)
    (cond ((null? lst) #f)
          (else
           (if (pred (car lst))
               n
               (list-index-with-n pred (cdr lst) (+ 1 n)))))))

(define list-index
  (lambda (pred lst)
    (list-index-with-n pred lst 0)))

(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))
