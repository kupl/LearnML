(* problem 2*)
let smallest_divisor : int->int
= fun n ->
let rec divisor a n =
if a==1 then n
else if (n mod a)==0 then a else divisor (a-1) n in
divisor (int_of_float (sqrt(float n))) n;;