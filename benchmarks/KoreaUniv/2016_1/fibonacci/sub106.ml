(* Problem 1 *)
let rec fib : int -> int
= fun n -> (* TODO *) 
	if n = 1 then 1
	else if n = 2 then 1
	else fib (n-1) + fib (n-2);;
