let rec fib : int -> int
= fun n -> 
if n = 0 then 1
else if n = 1 then 1
else (n * fib (n-1))
