(* Problem 1 *)
let rec fib n=
if n>=2 then fib(n-1)+fib(n-2)
else if n=1 then 1
else 0;;
