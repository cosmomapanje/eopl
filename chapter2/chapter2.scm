;;error
(define error
  (lambda (msg)
    (display msg)))

;;empty-env
(define empty-env
  (lambda ()
    (list 'empty-env)))

;;extend-env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;;apply-env
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (error 'no-bind))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (error 'invalid-env)))))
