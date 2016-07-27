;;; explicit-refs


;;;value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
	   (a-program (exp1)
		      (value-of exp1 (init-env))))))

;;;empty-store : () -> Sto
(define empty-store
  (lambda () '()))

(define the-store 'uninitialized)

;;;get-store : () -> Sto
(define get-store
  (lambda () the-store))

;;;initialize-store! : () -> Unspecified
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;;;reference? : SchemeVal -> Bool
(define reference?
  (lambda (v)
    (integer? v))
  
;;;newref : ExpVal -> Ref
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

;;;deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)))

;;;setref! : Ref * ExpVal -> Unspecified
(define setref!
  (lambda (ref val)
    (set! the-store
	  (letrec
	      ((setref-inner
		(lambda (store1 ref1)
		  (cond
		   ((null? store1)
		    (report-invalid-reference ref the-store))
		   ((zero? ref1)
		    (cons val (cdr store1)))
		   (else
		    (cons
		     (car store1)
		     (setref-inner
		      (cdr store1) (- ref1 1))))))))
	    (setref-inner the-store ref)))))


(define value-of
  (lambda ()
    (newref-exp (exp1)
		(let ((v1 (value-of exp1 env)))
		  (ref-val (newref v1))))
    (deref-exp (exp1)
	       (let ((v1 (value-of exp1 env)))
		 (let ((ref1 (expval->ref v1)))
		   (deref ref1))))
    (setref-exp (exp1 exp2)
		(let ((ref (expval->ref (value-of exp1 env))))
		  (let ((val2 (value-of exp2 env)))
		    (begin
		      (setref! ref val2)
		      (num-val 23)))))))
