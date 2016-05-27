;;; error
(define error
  (lambda (msg)
	(display msg)))

;;; 1.21 list-length
(define list-length
  (lambda (list)
	(cond ((null? list) 0)
		  (else
		   (+ 1 (list-length (cdr list)))))))

;;; 1.22 nth-element
(define nth-element
  (lambda (list num)
    (cond ((null? list) 
           (error 'out-of-range))
          (else
           (if (zero? num)
               (car list)
               (nth-element (cdr list) (- num 1)))))))

;;; 1.23 remove-first
(define remove-first
  (lambda (list a)
	(cond ((null? list) '())
		  ((eqv? (car list) a)
		   (cdr list))
		  (else
		   (cons (car list) (remove-first (cdr list) a))))))

;;; 1.24 occurs-free?
;;; LcExp ::= Identifier
;;;       ::= (lambda (Identifier) LcExp)
;;;       ::= (LcExp)
(define occurs-free?
  (lambda (sym exp)
    (cond ((null? exp) #t)
          ((symbol? exp)
           (eqv? sym exp))
          ((eqv? (car exp) 'lambda)
           (and (not (eqv? sym (car (car (cdr exp)))))
                (occurs-free? sym (car (cdr (cdr exp))))))
          (else
           (or (occurs-free? sym (car exp))
               (occurs-free? sym (car (cdr exp))))))))

;;; 1.25 subst
(define subst
  (lambda (new old list)
    (cond ((null? list) '())
          ((symbol? (car list))
           (if (eq? old (car list))
               (cons new (subst new old (cdr list)))
               (cons (car list) (subst new old (cdr list)))))
          (else
           (cons (subst new old (car list))
                 (subst new old (cdr list)))))))
