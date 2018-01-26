let rec fib n = if n<0 then raise (Failure "n cannot be negative") 
else if (n=0)||(n=1) then n else fib(n-1) + fib(n-2);;
