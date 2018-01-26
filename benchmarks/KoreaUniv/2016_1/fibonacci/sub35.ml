(* Problem 1 *)
let rec fib : int -> int
= fun n -> if(n<=1) then n
					 else fib(n-1) + fib (n-2)
