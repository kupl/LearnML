(* Problem 1 *)
let rec fib : int -> int
= fun n ->
(match n with
0 -> 0
|1 -> 1
|a -> fib (a-1) + fib (a-2) )
