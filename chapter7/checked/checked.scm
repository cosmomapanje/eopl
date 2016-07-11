#lang eopl
(define run-typechecker
  (lambda (string)
    (type-of-program (scan&parse string))))

(define type-of-program
  (lambda (pgm)
    (cases program pgm
		   (a-program (exp1) (type-of exp1 (init-tenv))))))

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (cond ((not (equal? ty1 ty2))
           (error 'not-equal-type-check)))))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
                 (list (type-to-external-form arg-type)
                       '->
                       (type-to-external-form result-type))))))

(define-datatype type type?
  (int-type)
  (bool-type)
  (proc-type
   (arg-type type?)
   (result-type type?)))

;(define int-type? number?)
(define bool-type?
  (lambda (x)
    (or (eqv? x #t)
        (eqv? x #f))))

(define type-of
  (lambda (exp tenv)
    (cases expression exp
		   (const-exp (num) (int-type))
		   (var-exp (var) (apply-tenv tenv var))
		   (diff-exp (exp1 exp2)
					 (let ((ty1 (type-of exp1 tenv))
						   (ty2 (type-of exp2 tenv)))
					   (check-equal-type! ty1 (int-type) exp1)
					   (check-equal-type! ty2 (int-type) exp2)
					   (int-type)))
		   (zero?-exp (exp1)
					  (let ((ty1 (type-of exp1 tenv)))
						(check-equal-type! ty1 (int-type) exp1)
						(bool-type)))
		   (if-exp (exp1 exp2 exp3)
				   (let ((ty1 (type-of exp1 tenv))
						 (ty2 (type-of exp2 tenv))
						 (ty3 (type-of exp3 tenv)))
					 (check-equal-type! ty1 (bool-type) exp1)
					 (check-equal-type! ty2 ty3 exp)
					 ty2))
		   (let-exp (var exp1 body)
					(let ((exp1-type (type-of exp1 tenv)))
					  (type-of body (extend-tenv var exp1-type tenv))))
		   (proc-exp (var var-type body)
					 (let ((result-type (type-of body (extend-tenv var  var-type tenv))))
					   (proc-type var-type result-type)))
		   (call-exp (exp1 exp2)
					 (let ((exp1-type (type-of exp1 tenv))
						   (exp2-type (type-of exp2 tenv)))
					   (cases type exp1-type
							  (proc-type (arg-type result-type)
										 (begin
										   (check-equal-type! arg-type exp2-type exp2)
										   result-type))
							  (else
							   (error 'call-exp-found-a-untyped)))))
		   (letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
					   (let ((tenv-for-letrec-body
							  (extend-tenv p-name
										   (proc-type b-var-type p-result-type)
										   tenv)))
						 (let ((p-body-type (type-of p-body
													 (extend-tenv b-var b-var-type tenv-for-letrec-body))))
						   (check-equal-type!
							p-body-type p-result-type p-body)
						   (type-of letrec-body tenv-for-letrec-body)))))))

(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (env environment?)))

;;; apply-procedure : proc * expval -> expval
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
		   (procedure (var body env)
					  (value-of body (extend-env var val env))))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))

;;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (val)
    (cases expval val
		   (num-val (num) num)
		   (else
			(error 'expval-error)))))

;;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
		   (bool-val (bool) bool)
		   (else
			(error 'expval-error)))))

;;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
		   (proc-val (proc) proc)
		   (else
			(error 'expval-error)))))

;;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
		   (a-program (exp1)
					  (value-of exp1 (init-env))))))

;;; value-of : Exp x Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
		   (const-exp (num) (num-val num))
		   (var-exp (var) (apply-env env var))
		   (diff-exp (exp1 exp2)
					 (let ((val1 (value-of exp1 env))
						   (val2 (value-of exp2 env)))
					   (let ((num1 (expval->num val1))
							 (num2 (expval->num val2)))
						 (num-val
						  (- num1 num2)))))
		   (zero?-exp (exp1)
					  (let ((val1 (value-of exp1 env)))
						(let ((num1 (expval->num val1)))
						  (if (zero? num1)
							  (bool-val #t)
							  (bool-val #f)))))
		   (if-exp (exp1 exp2 exp3)
				   (let ((val1 (value-of exp1 env)))
					 (if (expval->bool val1)
						 (value-of exp2 env)
						 (value-of exp3 env))))
		   (let-exp (var exp1 body)
					(let ((val1 (value-of exp1 env)))
					  (value-of body
								(extend-env var val1 env))))
		   (proc-exp (var ty body)
					 (proc-val (procedure var body env)))
		   (call-exp (exp1 exp2)
					 (let ((v1 (value-of exp1 env))
						   (v2 (value-of exp2 env)))
					   (let ((proc (expval->proc v1)))
						 (apply-procedure proc v2))))
		   (letrec-exp (ty1 p-name b-var ty2 p-body letrec-body)
					   (value-of letrec-body
								 (extend-env-rec p-name b-var p-body env))))))

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
    (expression ("proc" "(" identifier ":" type ")" expression) proc-exp)
    (expression ("(" expression expression")") call-exp)
    (expression ("letrec" type identifier "(" identifier ":" type ")" "=" expression "in" expression) letrec-exp)
    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("(" type "->" type ")") proc-type)))

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
   (var-type type?)
   (body expression?))
  (call-exp
   (exp1 expression?)
   (exp2 expression?))
  (letrec-exp
   (ret-type type?)
   (p-name identifier?)
   (b-var identifier?)
   (var-type type?)
   (p-body expression?)
   (letrec-body expression?)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; env
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extend-tenv-record
   (sym symbol?)
   (type type?)
   (tenv type-environment?)))

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type)
                 (extend-tenv 'v (int-type)
                              (extend-tenv 'i (int-type)
                                           (empty-tenv))))))
(define empty-tenv empty-tenv-record)
(define extend-tenv extend-tenv-record)

; init-env : () -> Env
(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

(define empty-env? null?)

(define extended-env->sym
  (lambda (e)
    (car (car e))))

(define extended-env->val
  (lambda (e)
    (car (cdr (car e)))))

(define extended-env->old-env
  (lambda (e)
    (cdr e)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
		   (empty-env ()
					  (error 'no-binding))
		   (extend-env (var val old-env)
					   (if (eqv? var search-var)
						   val
						   (apply-env old-env search-var)))
		   (extend-env-rec (p-name b-var body old-env)
						   (if (eqv? p-name search-var)
							   (proc-val (procedure b-var body env))
							   (apply-env old-env search-var))))))

(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
                         (error 'apply-tenv-unbound-variable))
      (extend-tenv-record (sym1 val1 old-env)
                          (if (eqv? sym sym1)
                              val1
                              (apply-tenv old-env sym))))))
(define error
  (lambda (msg)
    (display msg)))

;;; test
(define run&show
  (lambda (prog)
    (begin (display (run prog))
           (newline))))

(define runtc&show
  (lambda (prog)
    (begin (display (run-typechecker prog))
           (newline))))

(run&show "-(1, 2)")
(runtc&show "-(1, 2)")
(runtc&show "zero?(0)")
(runtc&show "zero?(1)")
(runtc&show "let f = proc (x:int) -(x, 11) in (f 77)")
