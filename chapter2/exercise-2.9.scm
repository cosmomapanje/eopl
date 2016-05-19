;;load for empty-env?
(load "exercise-2.8.scm")

;;error
(define error
  (lambda (msg)
    (display msg)))

;;has-binding
(define has-binding
  (lambda (env s)
    (cond
      ((empty-env? env)
       #f)
      (else
       (let ((saved-var (car (car env)))
             (saved-val (cdr (car env)))
             (saved-env (cdr env)))
         (if (eqv? save-var s)
             #t
             (has-binding saved-env s)))))))