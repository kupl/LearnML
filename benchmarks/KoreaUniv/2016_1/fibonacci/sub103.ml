(* Problem 1 *)
let rec fib : int -> int
= fun n -> (* TODO *)

match n with
|0 -> 0
|1 -> 1
|n -> fib(n-1) + fib(n-2)



