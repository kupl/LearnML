(* Problem 1 *)
let rec fib a =
if a == 0 then 1
else if a == 1 then 1
else fib (a-1) + fib (a-2);;
