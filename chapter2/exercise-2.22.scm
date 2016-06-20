;;; Exercise 2.22
(define-datatype stack-exp stack-exp?
  (empty-stack)
  (push-stack
   (var scheme-val?)
   (stack-exp stack-exp?))
  (pop-stack
   (stack-exp stack-exp?))
  (top-stack
   (stack-exp stack-exp?)))

(define scheme-val?
  (lambda (x)
	#t))
