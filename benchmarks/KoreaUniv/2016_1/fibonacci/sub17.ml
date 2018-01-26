exception Illegal_input

(* Problem 1 *)
let rec fib : int -> int
= fun n -> (* TODO *)
	if n<0 then raise Illegal_input
	else if n=0 then 0
	else if n=1 then 1
	else fib (n-1) + fib (n-2)
