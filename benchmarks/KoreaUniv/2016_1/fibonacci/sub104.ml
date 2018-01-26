(* Problem 1 *)
let rec fib (n : int) =
 match n with
 0 -> 0 | 1 -> 1 | _ -> fib (n-2) + fib (n-1);;
