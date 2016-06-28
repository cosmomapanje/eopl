#lang eopl
;;; Exercise 3.17
;;; let* operation
;;; 

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (emptylist-val)
  (cons-val
   (first expval?)
   (last expval?)))

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

;;; expval->car : ExpVal -> car of ExpVal
(define expval->car
  (lambda (val)
    (cases expval val
      (cons-val (first last)
                first)
      (else
       (error 'expval-error)))))

;;; expval->cdr : ExpVal -> cdr of ExpVal
(define expval->cdr
  (lambda (val)
    (cases expval val
      (cons-val (first last)
                last)
      (else
       (error 'expval-error)))))

;;; expval->emptylist?
(define expval->emptylist?
  (lambda (val)
    (cases expval val
      (emptylist-val ()
                     #t)
      (cons-val (first last)
                #f)
      (else
       (error 'not_list)))))

(define list-val
  (lambda (exps)
    (if (null? exps)
        (emptylist-val)
        (cons-val (car exps)
                  (list-val (cdr exps))))))

(define cond-val
  (lambda (exp1 exp2 env)
    (if (null? exp1)
        (error 'no-matched-in-cond)
        (let ((val1 (value-of (car exp1) env)))
          (let ((num1 (expval->bool val1)))
            (if (eqv? num1 #t)
                (value-of (car exp2) env)
                (cond-val (cdr exp1) (cdr exp2) env)))))))


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
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (equal?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (if (eqv? num1 num2)
                          (bool-val #t)
                          (bool-val #f)))))
      (greater?-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (if (> num1 num2)
                            (bool-val #t)
                            (bool-val #f)))))
      (less?-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 env))
                       (val2 (value-of exp2 env)))
                   (let ((num1 (expval->num val1))
                         (num2 (expval->num val2)))
                     (if (< num1 num2)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)
               (let ((vals (map (lambda (exp) (value-of exp env)) exp1)))
                 (value-of body
                           (extends-env var vals env))))
      (let*-exp (var exp1 body)
                (value-of body (extend*-env var exp1 env)))
      (minus-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (num-val
                      (- 0 num1)))))
      (cons-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (cons-val val1 val2)))
      (car-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (expval->car val1)))
      (cdr-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (expval->cdr val1)))
      (null?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((bool1 (expval->emptylist? val1)))
                     (bool-val bool1))))
      (emptylist-exp ()
                     (emptylist-val))
      (list-exp (exps)
                (list-val (map (lambda (exp) (value-of exp env)) exps)))
      (cond-exp (exp1 exp2)
                (cond-val exp1 exp2 env)))))

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
    (number ("-" digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("*" "(" expression "," expression ")") multi-exp)
    (expression ("/" "(" expression "," expression ")") quotient-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)))

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
  (zero?-exp
   (exp1 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var (list-of identifier?))
   (exp1 (list-of expression?))
   (body expression?))
  (let*-exp
   (var (list-of identifier?))
   (exp1 (list-of expression?))
   (body expression?))
  (minus-exp
   (exp1 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (null?-exp
   (exp1 expression?))
  (emptylist-exp)
  (list-exp
   (exps (list-of expression?)))
  (cond-exp
   (exp1 (list-of expression?))
   (exp2 (list-of expression?))))

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

(define extends-env
  (lambda (syms vals env)
    (if (null? syms)
        env
        (cons (list (car syms) (car vals)) (extends-env (cdr syms) (cdr vals) env)))))

(define extend-env
  (lambda (sym val env)
    (cons (list sym val) env)))

(define extend*-env
  (lambda (syms exps env)
    (if (null? exps)
        env
        (let ((env (cons (list (car syms) (value-of (car exps) env)) env)))
          (extend*-env (cdr syms) (cdr exps) env)))))

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
(run&show "zero?(5)")
(run&show "zero?(0)")
(run&show "equal?(1,2)")
(run&show "equal?(2,2)")
(run&show "greater?(3,5)")
(run&show "less?(3,5)")
(run&show "x")
(run&show "y")
(run&show "let y = 100 in equal?(10, y)")
(run&show "let y = 100 in equal?(100, y)")
(run&show "minus(6)")
(run&show "cons(3,4)")
(run&show "car(cons(3,4))")
(run&show "cons(3,emptylist)")
(run&show "let x = 4 in cons(x, cons(cons(-(x, 1), emptylist), emptylist))")
(run&show "let x = 4 in list(x, -(x,1), -(x,3))")
(run&show "let x = 5 in cond less?(x,3)==>3 equal?(x,3)==>5 zero?(x) ==> 0 greater?(x,4) ==> 4 end")
(run&show "let x = 30 in let x = -(x, 1) y = -(x, 2) in -(x, y)")
(run&show "let* x = 30 in let* x = -(x, 1) y = -(x, 2) in -(x, y)")
