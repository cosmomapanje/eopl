;; empty-stack
(define empty-stack
  (lambda ()
    '()))

(define empty-stack?
  (lambda (stack)
    (null? stack)))

(define push
  (lambda (item stack)
    (set cons item stack)))

(define pop
  (lambda (stack)
    (if (empty-stack? stack)
        (error '(nothing-in-stack))
        '())))

(define top
  (lambda (stack)
    (if (empty-stack? stack)
        (error 'nothing-in-stack)
        (car stack))))

(define error
  (lambda (msg)
    (display msg)))

;;; test

(define e (push 4 (push 5 (push 3 empty-stack))))
