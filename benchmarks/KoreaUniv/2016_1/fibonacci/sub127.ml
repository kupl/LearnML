(* Problem 1 *)
let rec fib : int -> int
= fun n ->if n>2 then fib(n-1) + fib(n-2)
					else 1;;
