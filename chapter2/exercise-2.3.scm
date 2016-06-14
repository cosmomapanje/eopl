;;; Exercise 2.3

(define zero
  '(diff (one) (one)))

(define is-zero?
  (lambda (diff-tree)
    (if (eqv? (calc (car (cdr diff-tree)))
              (calc (car (cdr (cdr diff-tree)))))
        #t
        #f)))

(define successor
  (lambda (diff-tree)
    (list (car diff-tree)
          (car (cdr diff-tree))
          (list 'diff (car (cdr (cdr diff-tree))) '(one)))))

(define predecessor
  (lambda (diff-tree)
    (list 'diff diff-tree '(one))))

(define calc
  (lambda (diff-tree)
    (cond ((null? diff-tree) 0)
          ((eqv? (car diff-tree) 'diff)
           (- (calc (car (cdr diff-tree)))
              (calc (car (cdr (cdr diff-tree))))))
          (else
           1))))

(define e
  '(diff (one)
         (diff (one)
               (diff (one)
                     (diff (one)
                           (one))))))
