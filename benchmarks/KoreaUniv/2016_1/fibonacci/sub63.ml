(* Problem 1 *)
let rec fib : int -> int
= fun n -> 
	if n < 0 then -1
	else match n with
	| 0 -> 0
	| 1 -> 1 
	| _ -> fib (n - 1) + fib (n - 2)