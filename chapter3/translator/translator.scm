#lang eopl
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
	(cond ((null? senv) (error 'report-undound-var))
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

;;; value-of-program : Nameless-program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-nameless-env))))))
;;; value-of : Nameless-exp * Nameless-exp -> ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 nameless-env))
                      (val2 (value-of exp2 nameless-env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 nameless-env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 nameless-env)))
                (if (expval->bool val1)
                    (value-of exp2 nameless-env)
                    (value-of exp3 nameless-env))))
      (call-exp (rator rand)
                (let ((v1 (value-of exp1 nameless-env))
                      (v2 (value-of exp1 nameless-env)))
                  (let ((proc (expval->proc v1)))
                    (apply-procedure proc v2))))
      (nameless-var-exp (n)
                        (apply-nameless-env nameless-env n))
      (nameless-let-exp (exp1 body)
                        (let ((val1 (value-of exp1 nameless-env)))
                          (value-of body
                                    (extend-nameless-env val nameless-env))))
      (nameless-proc-exp (body)
                         (proc-val
                          (procedure body nameless-env)))
      (else
       (report-invalid-translated-expression exp)))))

;;; run : String -> ExpVal
(define run
  (lambda (string)
	(value-of-program
	 (translation-of-program
	  (scan&parse string)))))


;;; nameless environment
;;; nameless-environment?
(define nameless-environment?
  (lambda (x)
	((list-of expval?) x)))

;;; empty-nameless-env
(define empty-nameless-env
  (lambda ()
	'()))

;;; extend-nameless-env
(define extend-nameless-env
  (lambda (val namess-env)
	(cons val Nameless-exp)))

;;; apply-nameless-env
(define apply-nameless-env
  (lambda (nameless-env n)
	(list-ref nameless-env n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sllgen
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define the-lexical-spec
  '((whitespcae (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression")") call-exp)
    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (call-exp
   (exp1 expression?)
   (exp2 expression?))
  (letrec-exp
   (p-name identifier?)
   (b-var identifier?)
   (p-body expression?)
   (letrec-body expression?))
  (nameless-var-exp
   (n))
  (nameless-let-exp
   (exp1)
   (body expression?))
  (nameless-proc-exp
   (body expression?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (id symbol?)
   (bvar symbol?)
   (body expression?)
   (saved-env environment?)))

(define identifier?
  symbol?)


;;;;;;;;;;;;;;;;;;; test
(define error
  (lambda (msg)
    (display msg)))

(define run&show
  (lambda (prog)
	(begin (display (run prog))
		   (newline))))

(run&show "-(1, 2)")
