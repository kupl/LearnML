(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with 0 | 1 -> n | _ -> fib (n-1) + fib (n-2)
