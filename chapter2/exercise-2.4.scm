;; empty-stack
(define empty-stack
  (lambda ()
    '()))

;; empty-stack?
(define empty-stack?
  (lambda (stack)
    (null? stack)))

;; push
(define push
  (lambda (item stack)
    (cons (list item) stack)))

;; pop
(define pop
  (lambda (stack)
    (cond ((null? stack)
           empty-stack)
          (else
           (set! stack (cdr stack))
           (car stack)))))

;; top
(define top
  (lambda (stack)
    '()))