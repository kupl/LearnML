(* Problem 1 *)
let rec fib : int -> int
= fun n -> if(n=1||n=2) then n-1 else fib(n-1)+fib(n-2);; (* TODO *)
