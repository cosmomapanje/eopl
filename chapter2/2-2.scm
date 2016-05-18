;;; 2.2.2
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
      (report-no-binding-found search-var))
     ((eqv? (car env) 'extend-env)
      (let ((saved-var (cadr env))
	    (saved-val (caddr env))
	    (saved-env (cadddr env)))
	(if (eqv? search-var saved-var)
	    saved-val
	    (apply-env saved-env search-var))))
     (else
      (report-invalid-env env)))))

; We need to write error handling
(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (error 'apply-env "Bad environment: ~s" env)))
