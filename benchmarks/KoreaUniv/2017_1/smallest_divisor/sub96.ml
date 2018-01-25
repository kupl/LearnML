(*problem 2*)
let smallest_divisor : int-> int
=fun n -> let rec division :int->int->int 
  =fun n i ->  if n mod 2 =0 then 2 
   else if i>int_of_float ( sqrt( float(n))) then n 
   else if n  mod i = 0 then i
   else division n (i+1) in division n 3
;;