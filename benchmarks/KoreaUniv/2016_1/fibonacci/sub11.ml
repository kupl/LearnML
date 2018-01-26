(* Problem 1 *)

let rec fib : int -> int
= fun num -> if num <= 0 then 0 else if num = 1 then 1 else fib(num-1) + fib(num-2);; 
