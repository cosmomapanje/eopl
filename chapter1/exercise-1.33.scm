;;; Exercise 1.33
(define mark-leaves-with-red-depth
  (let ((red-n 0))
    (lambda (btree)
      (cond ((leaf? btree) red-n)
            ((eqv? (contents-of btree) 'red)
             (begin (set! red-n (+ red-n 1))
                    (list 'red
                          (mark-leaves-with-red-depth (lson btree))
                          (mark-leaves-with-red-depth (rson btree)))))
            (else
             (list (contents-of btree)
                   (mark-leaves-with-red-depth (lson btree))
                   (mark-leaves-with-red-depth (rson btree))))))))

(define leaf
  (lambda (n)
    n))

(define interior-node
  (lambda (sym l r)
    (list sym l r)))

(define leaf?
  (lambda (l)
    (number? l)))

(define lson
  (lambda (l)
    (if (leaf? l)
        '()
        (car (cdr l)))))

(define rson
  (lambda (l)
    (if (leaf? l)
        '()
        (car (cdr (cdr l))))))

(define contents-of
  (lambda (l)
    (if (leaf? l)
        l
        (car l))))

;;; test
(define e
  (mark-leaves-with-red-depth
   (interior-node
    'red
    (interior-node
     'bar
     (leaf 26)
     (leaf 12))
    (interior-node
     'red
     (leaf 11)
     (interior-node
      'quux
      (leaf 117)
      (leaf 14))))))
