let rec prime n =
if n = 0 || n = 1 then false
else if n = 2 then true
else let rec prime_1 x = if x*x > n then true
else (n mod x) <> 0 && prime_1 (x+1)
in prime_1 2