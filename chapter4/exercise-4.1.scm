;;; Exercise 4.1

let g = proc (dummy)
          let counter = newref(0)
          in begin
              setref(counter, -(deref(counter), -1));
              deref(counter)
             end
in let a = (g 11)
   in let b = (g 11)
      in -(a, b)

(num-val 0)
