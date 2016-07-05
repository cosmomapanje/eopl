;;; Exercise 7.1
;;1
;;(int -> int)
proc (x) -(x, 3)

;;2
;;((t -> t) -> (t -> int))
proc (f) proc (x) -((f x), 1)

;;3
;;(t -> t)
proc (x) x

;;4
;;((t -> t) -> (t -> t))
proc (x) proc (y) (x y)

;;5
;;((int -> t) -> t)
proc (x) (x 3)

;;6
proc (x) (x x)

;;7
;;(bool -> int)
proc (x) if x then 88 else 99

;;8
;;(bool -> (int -> int))
proc (x) proc (y) if x then y else 99

;;9
;;int
(proc (p) if p then 88 else 99
	  33)

;;10
;;(bool -> int)
(proc (p) if p then 88 else 99
	  proc (z) z)

;;11
;;
proc (f) proc (g) proc (p) proc (x) if (p (f x)) then (g 1) else -((f x), 1)

;;12
proc (x) proc (p) proc (f) if (p x) then -(x, 1) else (f p)

;;13
proc (f) let d = proc (x) proc (z) ((f (x x)) z) in proc (n) ((f (d d)) n)
