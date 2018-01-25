let rec smallest_divisor : int -> int = fun n ->
	if n<0 then raise (Failure "n is negative number.")
	else if n mod 2 = 0 then 2
  else let x = n in
       let rec f x i = 
         if i > int_of_float (sqrt (float_of_int x)) then x
         else if x mod i = 0 then i
         else f x (i+2) in f x 3;;