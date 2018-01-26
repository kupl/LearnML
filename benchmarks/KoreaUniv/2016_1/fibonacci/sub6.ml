exception Invalid;;

(* Problem 1 *)
let rec fib : int -> int
= fun n -> 
	match n with
		x when x < 0 -> raise Invalid|
		0 -> 0|
		1 -> 1|
		_ -> fib(n-1) + fib(n-2);;
