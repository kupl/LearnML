(* problem 2*)

let smallest_divisor : int -> int = fun n ->
  let a = int_of_float(sqrt(float_of_int n)) in
  let rec f i = 
  	if a < i then n
  	else if n mod i = 0 then i
  	else f (i+1) in
  f 2;;