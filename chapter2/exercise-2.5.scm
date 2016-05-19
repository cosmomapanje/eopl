;;error
(define error
  (lambda (msg)
    (display msg)))

;;empty-env
(define empty-env
  (lambda ()
    '()))

;;extend-env
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

;;apply-env
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? env empty-env)
       (error 'no-bind))
      (else
       (let ((saved-var (car (car env)))
             (saved-val (cdr (car env)))
             (saved-env (cdr env)))
         (if (eqv? saved-var search-var)
             saved-val
             (apply-env saved-env search-var)))))))
  