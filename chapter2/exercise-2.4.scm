;;; Exercise 2.4
(empty-stack) = ()
(push s stack) = (s stack)
(pop stack) = (stack-without-first)
(top stack) = (car stack)
(empty-stack? stack) = #t if stack = ()

;;; constructors: empty-stack, push, pop
;;; observers: top empty-stack?
