;;; implicit-refs
#lang eopl
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
                 (value-of body
                           (extend-env var (newref val) env))))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?))
  (ref-val
   (ref reference?)))

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

;;; expval->ref : ExpVal -> Ref
(define expval->ref
  (lambda (v)
    (cases expval v
      (ref-val (ref) ref)
      (else
       (error 'expval-error)))))

;;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;;; value-of : Exp x Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (deref (apply-env env var)))
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
                           (extend-env var (newref val1) env))))
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      (call-exp (exp1 exp2)
                (let ((v1 (value-of exp1 env))
                      (v2 (value-of exp2 env)))
                  (let ((proc (expval->proc v1)))
                    (apply-procedure proc v2))))
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of letrec-body
                            (extend-env-rec p-name b-var p-body env)))
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
                        (num-val 23)))))
      (begin-exp (exp1 exps)
                 (letrec
                     ((value-of-begin-exps
                       (lambda (e1 es)
                         (let ((v1 (value-of e1 env)))
                           (if (null? es)
                               v1
                               (value-of-begin-exps (car es) (cdr es)))))))
                   (value-of-begin-exps exp1 exps)))
      (set-exp (var rhs)
               (setref!
                (apply-env env var)
                (value-of rhs env))
               (num-val 27)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; store
;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (integer? v)))

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
                    (error 'invalid-reference))
                   ((zero? ref1)
                    (cons val (cdr store1)))
                   (else
                    (cons
                     (car store1)
                     (setref-inner
                      (cdr store1) (- ref1 1))))))))
                   (setref-inner the-store ref)))))
  
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
    (expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression) letrec-exp)
    (expression ("newref" "(" expression ")") newref-exp)
    (expression ("deref" "(" expression ")") deref-exp)
    (expression ("setref" "(" expression "," expression ")") setref-exp)
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
    (expression ("set" identifier "=" expression) set-exp)))

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
   (p-name (list-of identifier?))
   (b-var (list-of identifier?))
   (p-body (list-of expression?))
   (letrec-body expression?))
  (newref-exp
   (exp1 expression?))
  (deref-exp
   (exp1 expression?))
  (setref-exp
   (exp1 expression?)
   (exp2 expression?))
  (begin-exp
   (exp1 expression?)
   (exps (list-of expression?)))
  (set-exp
   (var identifier?)
   (rhs expression?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval reference?)
   (saved-env environment?))
  (extend-env-rec
   (id (list-of symbol?))
   (bvar (list-of symbol?))
   (body (list-of expression?))
   (saved-env environment?))
)

(define identifier?
  symbol?)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; env
;;;;;;;;;;;;;;;;;;;;;;;;;;

; init-env : () -> Env
(define init-env
  (lambda ()
    (extend-env
     'i (newref (num-val 1))
     (extend-env
      'v (newref (num-val 5))
      (extend-env
       'x (newref (num-val 10))
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

(define has-proc?
  (lambda (symbol names vars bodys)
    (if (null? names)
        (list #f)
        (if (eqv? symbol (car names))
            (list #t (car vars) (car bodys))
            (has-proc? symbol (cdr names)
                       (cdr vars)
                       (cdr bodys))))))
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
                      (if (null? p-name)
                          (apply-env old-env search-var)
                          (let ((res (has-proc? search-var p-name b-var body)))
                            (if (car res)
                                (newref
                                 (proc-val (procedure (cadr res) (caddr res) env)))
                                (apply-env old-env search-var))))))))

(define extend-env-rec*
  (lambda (p-name b-var body old-env)
    (if (null? p-name)
        old-env
        (let ((env (extend-env-rec (car p-name) (car b-var) (car body) old-env)))
          extend-env-rec*
          (cdr p-name)
          (cdr b-var)
          (cdr body)
          env))))

(define error
  (lambda (msg)
    (begin (display msg)
           (newline))))

;;; test
(define run&show
  (lambda (prog)
    (begin (display (run prog))
           (newline))))
(run&show "11")
(run&show "-33")
(run&show "-(1, 2)")

(run&show "if zero?(0) then 0 else 1")
(run&show "if zero?(5) then 0 else 1")

(run&show "let x = 20 in x")
(run&show "let x = 3 in -(x, 1)")
(run&show "let x = -(4, 1) in -(x, 1)")
(run&show "let x = 3 in let y = 4 in -(x, y)")
(run&show "let x = 3 in let x = 4 in x")
(run&show "let x = 3 in let x = -(x, 1) in x")

(run&show "((proc (x) proc(y) -(x, y) 5) 6)")
(run&show "let f = proc(x) proc(y) -(x,y) in ((f -(10,5)) 6)")

(run&show "letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 6)")
(run&show "letrec
             even(x) = if zero?(x) then 1 else (odd -(x, 1))
             odd(x)  = if zero?(x) then 0 else (even -(x, 1))
           in (odd 12)")
(run&show "let g = proc (dummy)
                     let counter = newref(0)
                     in begin
                         setref(counter, -(deref(counter), -1));
                         deref(counter)
                        end
           in let a = (g 11)
              in let b = (g 11)
                 in -(a, b)")

(run&show "let g = let counter = newref(0)
                   in proc (dummy)
                       begin
                        setref(counter, -(deref(counter), -1));
                        deref(counter)
                       end
           in let a = (g 11)
              in let b = (g 11)
                 in -(a, b)")

(run&show "let x = 0 in begin set x = 27; x end")

(run&show "let g = let count = 0 in proc(d)
                                  let d = set count = -(count, -1)
                                  in count
           in -((g 11), (g 22))")

(run&show "let f = proc (x) proc (y)
                             begin
                              set x = -(x, -1);
                              -(x, y)
                             end
           in ((f 44) 33)")
