;;; Exercise 2.31
#lang eopl
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define translation
  (lambda (prog)
    (cond ((null? prog) '())
          ((eqv? (car prog) '-)
           (cons 'diff-exp
                 (translation (cdr prog))))
          ((number? (car prog))
           (cons (list 'const-exp
                       (car prog))
                 (translation (cdr prog))))
          (else
           (display 'error)))))

(define e (diff-exp
           (diff-exp
            (const-exp 3)
            (const-exp 2))
           (diff-exp
            (const-exp 4)
            (diff-exp
             (const-exp 12)
             (const-exp 7)))))

(define runtest
  (lambda (prog)
    (begin
      (display (translation prog))
      (newline))))

;;; test
(runtest '(- 1 2))
(runtest '(- - 3 2 - 4 - 12 7))
