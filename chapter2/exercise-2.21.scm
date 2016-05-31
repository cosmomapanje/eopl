;;; Exercise 2.21
;;; Env-exp ::= (empty-env)
;;;         ::= (extend-env Identifier Scheme-value Env-exp)

;;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;;; Var = Sym

(define-datatype env env?
  ())
