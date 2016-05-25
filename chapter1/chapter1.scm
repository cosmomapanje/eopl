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
(define occurs-free?
  (lambda (var exp)))

;;; 1.25 subst
