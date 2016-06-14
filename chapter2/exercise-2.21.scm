;;; Exercise 2.21
;;; Env-exp ::= (empty-env)
;;;         ::= (extend-env Identifier Scheme-value Env-exp)

;;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;;; Var = Sym

(define error
  (lambda (msg)
    (display msg)))

(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
   (var env-var?)
   (val scheme-val?)
   (env env-exp?)))

(define scheme-val?
  (lambda (x)
	#t))

(define env-var? symbol?)

(define apply-env
  (lambda (environment search-var)
    (cases env-exp environment
		   (empty-env ()
					  (error 'no-binding-found))
		   (extend-env (var val env)
					   (if (eqv? search-var var)
						   val
						   (apply-env env search-var))))))

(define has-binding?
  (lambda (environment search-var)
	(cases env-exp environment
		   (empty-env ()
					  #f)
		   (extend-env (var val env)
					   (if (eqv? search-var var)
						   #t
						   (has-binding? env search-var))))))
