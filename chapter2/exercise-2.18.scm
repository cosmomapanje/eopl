;;; Exercise 2.18
;;; Definition
;;; NodeInSequence ::= (Int Listof(Int) Listof(Int))
(define error
  (lambda (msg)
    (display msg)))

(define number->sequence
  (lambda (num)
    (list num '() '())))

(define current-element
  (lambda (seq)
    (car seq)))

(define move-to-left
  (lambda (seq)
    (cond ((at-left-end? seq) (error 'at-left-end))
          (else
           (list (car (car (cdr seq)))
                 (cdr (car (cdr seq)))
                 (cons (car seq) (car (cdr (cdr seq)))))))))

(define move-to-right
  (lambda (seq)
    (cond ((at-right-end? seq) (error 'at-right-end))
          (else
           (list (car (car (cdr (cdr seq))))
                 (cons (car seq) (car (cdr seq)))
                 (cdr (car (cdr (cdr seq)))))))))

(define insert-to-left
  (lambda (num seq)
    (list (car seq)
          (cons num (car (cdr seq)))
          (car (cdr (cdr seq))))))

(define insert-to-right
  (lambda (num seq)
    (list (car seq)
          (car (cdr seq))
          (cons num (car (cdr (cdr seq)))))))

(define at-left-end?
  (lambda (seq)
    (null? (car (cdr seq)))))

(define at-right-end?
  (lambda (seq)
    (null? (car (cdr (cdr seq))))))

;;test case
(define e '(6 (5 4 3 2 1) (7 8 9)))
e
(number->sequence 7)
(current-element e)
(move-to-left e)
(move-to-right e)
(insert-to-left 13 e)
(insert-to-right 13 e)
