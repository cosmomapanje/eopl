;;; Exercise 2.15
;;; var-exp
;;; lambda-exp
;;; app-exp

;;; var-exp?
;;; lambda-exp?
;;; app-exp?
(define var-exp?
  (lambda (lc-exp)
	(symbol? lc-exp)))

(define lambda-exp?
  (lambda (lc-exp)
	(eqv? (car lc-exp) 'lambda)))

(define app-exp?
  (lambda (lc-exp)
	(and (not (var-exp? lc-exp))
		 (not (lambda-exp? lc-exp)))))

;;; var-exp->var
;;; lambda-exp->bound-var
;;; lambda-exp->body
;;; app-exp->rator
;;; app-exp->rand
(define var-exp->var
  (lambda (lc-exp)
	lc-exp))

(define lambda-exp->bound-var
  (lambda (lc-exp)
	(car (car (cdr lc-exp)))))

(define lambda-exp->body
  (lambda (lc-exp)
	(car (cdr (cdr lc-exp)))))

(define app-exp->rator
  (lambda (lc-exp)
	(car lc-exp)))

(define app-exp->rand
  (lambda (lc-exp)
	(car (cdr lc-exp))))

(define occurs-free?
  (lambda (search-var exp)
	(cond
	 ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
	 ((lambda-exp? exp)
	  (and
	   (not (eqv? search-var (lambda-exp->bound-var exp)))
	   (occurs-free? search-var (lambda-exp->body exp))))
	 (else
	  (or (occurs-free? search-var (app-exp->rator exp))
		  (occurs-free? search-var (app-exp->rand exp)))))))
