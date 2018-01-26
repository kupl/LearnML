(* Problem 1 *)
let rec fib (n : int) : int =
match n with
0 -> 0
|1 -> 1
|_ -> fib (n - 1) + fib (n - 2);;
