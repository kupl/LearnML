(* Problem 1 *)
exception Problem

let rec fib n =
 
	if n<0 then raise Problem
	else

match n with
0 -> 0
|1 -> 1
|_ -> fib (n-1) + fib (n-2);;
