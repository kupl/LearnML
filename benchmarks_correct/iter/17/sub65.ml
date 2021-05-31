let rec iter (n, f) (arg : 'a) : 'a = 
if n <= 0 then arg
else if n == 1 then f arg
else iter(n-1, f)(f arg)
