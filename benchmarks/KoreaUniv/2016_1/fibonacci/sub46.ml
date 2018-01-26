(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0 
  if n < 3 then
 	1
 	else
	fibonacci (n-1) + fibonacci (n-2)
