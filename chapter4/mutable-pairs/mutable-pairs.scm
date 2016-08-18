;;; Mutable-pairs
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
   (ref reference?))
  (mutpair-val
   (mp mutpair?)))

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

;;; expval->mutpair : ExpVal -> Mutpair
(define expval->mutpair
  (lambda (v)
    (cases expval v
      (mutpair-val (mp) mp)
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
               (num-val 27))
      (newpair-exp (exp1 exp2)
                   (let ((val1 (value-of exp1 env))
                         (val2 (value-of exp2 env)))
                     (mutpair-val (make-pair val1 val2))))
      (left-exp (mp)
                (let ((val (value-of mp env)))
                  (let ((p1 (expval->mutpair val)))
                    (left p1))))
      (right-exp (mp)
                 (let ((val (value-of mp env)))
                   (let ((p1 (expval->mutpair val)))
                     (right p1))))
      (setleft-exp (mp exp)
                   (let ((mp-val (value-of mp env))
                         (val (value-of exp env)))
                     (let ((p (expval->mutpair mp-val)))
                       (begin
                         (setleft p val)
                         (num-val 82)))))
      (setright-exp (mp exp)
                    (let ((mp-val (value-of mp env))
                          (val (value-of exp env)))
                      (let ((p (expval->mutpair mp-val)))
                        (begin
                          (setright p val)
                          (num-val 83))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mutable-pairs
(define-datatype mutpair mutpair?
  (a-pair
   (left-loc reference?)
   (right-loc reference?)))

;;; make-pair(newpair) : expval * expval -> mutpair
(define make-pair
  (lambda (val1 val2)
    (a-pair
     (newref val1)
     (newref val2))))

;;; left : mutpair -> expval
(define left
  (lambda (mp)
    (cases mutpair mp
      (a-pair (left-loc right-loc)
              (deref left-loc)))))

;;; right : mutpair -> expval
(define right
  (lambda (mp)
    (cases mutpair mp
      (a-pair (left-loc right-loc)
              (deref right-loc)))))

;;; setleft: mutpair * expval -> unspecified
(define setleft
  (lambda (mp val)
    (cases mutpair mp
      (a-pair (left-loc right-loc)
              (setref! left-loc val)))))

;;; setright: mutpair * expval -> unspecified
(define setright
  (lambda (mp val)
    (cases mutpair mp
      (a-pair (left-loc right-loc)
              (setref! right-loc val)))))


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
    (expression ("set" identifier "=" expression) set-exp)
    (expression ("pair" "(" expression "," expression ")") newpair-exp)
    (expression ("left" "(" expression ")") left-exp)
    (expression ("right" "(" expression ")") right-exp)
    (expression ("setleft" "(" expression "," expression ")") setleft-exp)
    (expression ("setright" "(" expression "," expression ")") setright-exp)))

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
   (rhs expression?))
  (newpair-exp
   (exp1 expression?)
   (exp2 expression?))
  (left-exp
   (mp expression?))
  (right-exp
   (mp expression?))
  (setleft-exp
   (mp expression?)
   (exp expression?))
  (setright-exp
   (mp expression?)
   (exp expression?)))

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

(run&show "pair(11,22)")
(run&show "right(pair(11,22))")
(run&show "left(pair(11,22))")
(run&show "setleft(pair(11,22), 33)")
(run&show "setright(pair(11,22), 33)")

(run&show "let glo = pair(11,22)
           in let f = proc(loc)
                       let d1 = setright(loc, left(loc))
                       in let d2 = setleft(glo, 99)
                       in -(left(loc), right(loc))
           in (f glo)")
