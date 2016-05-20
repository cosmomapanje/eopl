;;error
(define error
  (lambda (msg)
    (display msg)))

;;empty-env
(define empty-env
 (lambda ()
   (quote ())))

;;extend-env*
(define extend-env*
  (lambda (vars vals env)
    (cond
      ((list? vars)
       (if (list? vals)
           (cons (cons vars vals) env)
           (error 'args-doesnt-match)))
      (else
       (cons (cons (list vars) (list vals)) env)))))

;;search-in-list
(define search-in-list
  (lambda (var list-vars list-vals)
    (cond 
      ((null? list-vars)
       (cons #f '()))
      ((eqv? (car list-vars) var)
       (cons #t (car list-vals)))
      (else
       (search-in-list var (cdr list-vars) (cdr list-vals))))))

;;apply-env
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? env empty-env)
       (error 'no-bind))
      (else
       (let ((saved-vars (car (car env)))
             (saved-vals (cdr (car env)))
             (saved-env (cdr env)))
         (let ((searched (search-in-list search-var saved-vars saved-vals)))
           (if (car searched)
               (cdr searched)
               (apply-env saved-env search-var))))))))

;;
(cons (cons '(1 2 3) '(x y z)) '('(a) 1))
(cons (list 1) (list 'a))
(define e (extend-env* '(a b c) '(1 2 3)
                       (extend-env* 'd 4
                                    (extend-env* '(e f) '(5 6) empty-env))))
(extend-env* 'a 1 empty-env)
