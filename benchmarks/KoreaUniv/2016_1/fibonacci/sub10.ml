(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n <= 1 then
        if n = 0 then 0 else 1
        else fib (n-1) + fib(n-2);;
