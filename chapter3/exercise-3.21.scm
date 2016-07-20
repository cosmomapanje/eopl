;;; exercise 3.21
#lang eopl
;;; proc? : Scheme  value -> boolean
;;; procedure : vble * expression * env -> proc
(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (env environment?)))

(define environment?
  (lambda (e)
    (or (null? e)
        (and (pair? e)
             (symbol? (car (car e)))
             (expval? (car (cdr (car e))))
             (environment? (cdr e))))))

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
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      (call-exp (exp1 exp2)
                (let ((v1 (value-of exp1 env))
                      (v2 (value-of exp2 env)))
                  (let ((proc (expval->proc v1)))
                    (apply-procedure proc v2))))
      (letproc-exp (name var exp body)
                   (let ((p-val (proc-val (procedure var exp env))))
                     (value-of body (extend-env name p-val env)))))))


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
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    (expression ("letproc" identifier "(" identifier ")" "=" expression "in" expression) letproc-exp)))

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
   (var (list-of identifier?))
   (body expression?))
  (call-exp
   (exp1 expression?)
   (exp2 (list-of expression?)))
  (letproc-exp
   (name identifier?)
   (var identifier?)
   (exp expression?)
   (body expression?)))

(define identifier?
  symbol?)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; env
;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define empty-env
  (lambda ()
    '()))

(define empty-env? null?)

(define extend-env
  (lambda (sym val env)
    (cons (list sym val) env)))

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
  (lambda (env search-sym)
    (if (empty-env? env)
        (error 'no-binding)
        (let ((sym (extended-env->sym env))
              (val (extended-env->val env))
              (old-env (extended-env->old-env env)))
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))
(define error
  (lambda (msg)
    (display msg)))

;;; test
(define run&show
  (lambda (prog)
    (begin (display (run prog))
           (newline))))

(run&show "let f = proc (x) -(x,1) in (f 3)")
(run&show "let x = 200
           in let f = proc (z) -(z, x)
              in let x = 100
                 in let g = proc (z) -(z, x)
                    in -((f 1), (g 1))")
(run&show "let sum = proc(x) proc(y) -(x, -(0,y)) in ((sum 10) 3)")
;;; letproc id (id) = exp in exp
(run&show "letproc sub  (x) = -(x, 1) in -((sub 5), 4)")
