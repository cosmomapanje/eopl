let makemult = proc (maker)
                 proc (x)
                   if zero?(x)
                   then 0
                   else -(((maker maker) -(x,1)), -4)
in let times4 = proc (x) ((makemult makemult) x)
   in (times4 3)


((makemult makemult) 3)
===>
((proc (x)
	   if zero?(x)
	   then 0
	   else -(((makemult makemult) -(x, 1)), -4)) 3)
===>
-(((makemult makemult) 2), -4)
==>
-(-(((makemult makemult) 1), -4), -4)
===>
-(-(-(((makemult makemult) 0), -4), -4), -4)
===>
-(-(-(0, -4), -4), -4)
===>
-(-(4, -4), -4)
===>
-(8, -4)
===>
12
