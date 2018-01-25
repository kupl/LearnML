let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f
let rec iter (n,f) a =
   if n=0 then a else f (iter(n-1,f) a);;

