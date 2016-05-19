(load "exercise-2.5.scm")

;;extend-env*
(define extend-env*
  (lambda (vars vals env)
    (cond
      ((null? vars)
       (if (null? vals)
           (empty-env)
           (error 'args-number-does-match)))
      ((null? vals)
       (error 'args-number-does-match))
      (else
         (extend-env
          (car vars)
          (car vals)
          (extend-env* (cdr vars) (cdr vals) env))))))
