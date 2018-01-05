let rec iter (x : int * ( 'a -> 'a )) (arg : 'a) : 'a = 
let (n, f) = x in
if n <= 0 then arg
else if n == 1 then f arg
else iter(n-1, f)(f arg)
