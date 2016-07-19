;;; translator

;;; empty-senv : () -> Senv
(define empty-senv
  (lambda ()
	'()))

;;; extend-senv : Var * Senv -> Senv
(define extend-senv
  (lambda (var senv)
	(cons var senv)))

;;; apply-senv : Senv * Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
	(cond ((null? senv) (report-undound-var var))
		  ((eqv? var (car senv)) 0)
		  (else
		   (+ 1 (apply-senv (cdr senv) var))))))



;;; translation-of-program : Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
	(cases program pgm
		   (a-program (exp1)
					  (a-program (translation-of exp1 (init-senv)))))))

;;; init-senv : () -> Senv
(define init-senv
  (lambda ()
	(extend-senv 'i
				 (extend-senv 'v
							  (extend-senv 'x
										   (empty-senv))))))

;;; translation-of : Exp * Senv -> Nameless-exp
(define translation-of
  (lambda (exp senv)
	(cases expression exp
		   (const-exp (num)
					  (const-exp num))
		   (diff-exp (exp1 exp2)
					 (diff-exp
					  (translation-of exp1 senv)
					  (translation-of exp2 senv)))
		   (zero?-exp (exp1)
					  (zero?-exp
					   (translation-of exp1 senv)))
		   (if-exp (exp1 exp2 exp3)
				   (if-exp
					(translation-of exp1 senv)
					(translation-of exp2 senv)
					(translation-of exp3 senv)))
		   (var-exp (var)
					(nameless-var-exp
					 (apply-senv senv var)))
		   (let-exp (var exp1 body)
					(nameless-let-exp
					 (translation-of exp1 senv)
					 (translation-of body (extend-senv var senv))))
		   (proc-exp (var body)
					 (nameless-proc-exp
					  (translation-of body
									  (extend-senv var senv))))
		   (call-exp (rator rand)
					 (call-exp
					  (translation-of rator senv)
					  (translation-of rand senv)))
		   (else
			(report-invalid-source-expression exp)))))

;;; run : String -> ExpVal
(define run
  (lambda (string)
	(value-of-program
	 (translation-of-program
	  (scan&parse string)))))
