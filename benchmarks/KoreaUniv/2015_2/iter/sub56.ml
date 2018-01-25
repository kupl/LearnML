let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
if n=0 then (fun a->a) 
else (fun a -> iter(n-1, f) (f a)) 
