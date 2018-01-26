(*HW 1*)
let is_Zero n = if n=0 then true else false;;
let is_One n = if n=1 then true else false;;
let check n = if n=1 then 1 else 0;;
let rec fib n = if (is_Zero n) || (is_One n) then check n else (fib(n-1) + fib(n-2));;
