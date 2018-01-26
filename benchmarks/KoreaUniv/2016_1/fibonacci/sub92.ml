(* Problem 1 *)(*suppose fib 0=0. 0,1,1,2,3... *)
let rec fib : int -> int
= fun n -> if n=0 then 0
					 else if n=1 then 1
					 else fib(n-1)+fib(n-2);;
