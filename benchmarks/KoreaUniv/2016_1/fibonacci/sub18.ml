(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n=0 then 0
	else if n=1 then 1
	else fib (n-1) + fib (n-2);;
