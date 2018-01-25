let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f 
(let rec iter f a n = 
if (n = 0) then 
a else 
(f n (iter f a (n-1)))

)
