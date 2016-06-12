;;; Exercise 2.1
;;; Bigits
(define BASE 16)

(define zero
    '())

(define is-zero?
  (lambda (bigits)
    (null? bigits)))

(define successor
  (lambda (bigits)
    (cond ((is-zero? bigits) (cons 1 bigits))
          (else
           (if (eqv? (- BASE 1) (car bigits))
               (cons 0 (successor (cdr bigits)))
               (cons (+ 1 (car bigits)) (cdr bigits)))))))

(define predecessor
  (lambda (bigits)
    (cond ((is-zero? bigits) (error 'not-support-negative-number))
          (else
           (if (eqv? (car bigits) 0)
               (cons (- BASE 1) (predecessor (cdr bigits)))
               (if (and (eqv? (car bigits) 1) (null? (cdr bigits)))
                   '()
                   (cons (- (car bigits) 1) (cdr bigits))))))))

(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        (plus (predecessor x) (successor y)))))

(define minus
  (lambda (x y)
    (cond ((and (is-zero? x)
                (not (is-zero? y)))
           (error 'not-support-negative-number))
          ((and (is-zero? x)
                (is-zero? y))
           zero)
          (else
           (if (is-zero? y)
               x
               (minus (predecessor x) (predecessor y)))))))

(define multi
  (lambda (x y)
    (cond ((is-zero? y) zero)
          ((is-zero? x) zero)
          (else
           (if (is-zero? (predecessor y))
               x
               (plus x (multi x (predecessor y))))))))

(define factorial
  (lambda (x)
    (cond ((is-zero? (predecessor x)) x)
          (else
           (multi x (factorial (predecessor x)))))))

(define error
  (lambda (msg)
    (display msg)))

;;; test
(factorial '(10))
