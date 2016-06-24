;;; Exercise 3.14
#lang eopl

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

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
      (add-exp (exp1 exp2)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val
                    (+ num1 num2)))))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (multi-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 env))
                       (val2 (value-of exp2 env)))
                   (let ((num1 (expval->num val1))
                         (num2 (expval->num val2)))
                     (num-val
                      (* num1 num2)))))
      (quotient-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (num-val
                         (/ num1 num2)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of-bool-exp exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      (minus-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (num-val
                      (- 0 num1))))))))

(define make-table
  (lambda lst
    (lambda (op)
      (cdr (assoc op lst)))))

(define entry cons)

(define unary-bool-table
  (make-table (entry "zero?" zero?)))

(define binary-bool-table
  (make-table (entry "equal?" =)
              (entry "greater?" >)
              (entry "less?" <)))

(define look-up-unary-bool-op
  (lambda (op)
    (cond ((eqv? op "zero?") zero?)
          (else
           (error 'not-found-in-unary-bool-op)))))

(define look-up-binary-bool-op
  (lambda (op)
    (cond ((eqv? op "equal?") =)
          ((eqv? op "greater?") >)
          ((eqv? op "less?") <)
          (else
           (error 'not-found-in-binary-bool-op)))))


;;; value-of-bool-exp
(define value-of-bool-exp
  (lambda (boolexp env)
    (cases bool-exp boolexp
      (unary-bool-exp (op1 exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1)))
                          (bool-val
                           ((unary-bool-table op1) num1)))))
      (binary-bool-exp (op1 exp1 exp2)
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
                         (let ((num1 (expval->num val1))
                               (num2 (expval->num val2)))
                           (bool-val
                            ((binary-bool-table op1) num1 num2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sllgen
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define the-lexical-spec
  '((whitespcae (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (unary-bool-op ("zero?") string)
    (binary-bool-op ((or "equal?" "greater?" "less?")) string)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("*" "(" expression "," expression ")") multi-exp)
    (expression ("/" "(" expression "," expression ")") quotient-exp)
    (bool-exp (unary-bool-op "(" expression ")") unary-bool-exp)
    (bool-exp (binary-bool-op "(" expression "," expression ")") binary-bool-exp)
    (expression ("if" bool-exp "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("minus" "(" expression ")") minus-exp)))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (multi-exp
   (exp1 expression?)
   (exp2 expression?))
  (quotient-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp
   (exp1 bool-exp?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (minus-exp
   (exp1 expression?)))

(define-datatype bool-exp bool-exp?
  (unary-bool-exp
   (op1 string?)
   (exp1 expression?))
  (binary-bool-exp
   (op1 string?)
   (exp1 expression?)
   (exp2 expression?)))

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

(define run&show
  (lambda (prog)
    (begin (display (run prog))
	   (newline))))


;;; test
(run&show "5")
(run&show "-(5,3)")
(run&show "+(5,3)")
(run&show "*(5,3)")
(run&show "/(5,3)")
(run&show "if equal?(5,0) then 1 else 2")
(run&show "if zero?(5) then 1 else 2")
(run&show "if zero?(0) then 1 else 2")
(run&show "x")
(run&show "y")
(run&show "let y = 100 in if equal?(10, y) then -(y, 0) else -(y, 10)")
(run&show "let y = 10 in if equal?(10, y) then -(y, 0) else -(y, 10)")
(run&show "minus(6)")
