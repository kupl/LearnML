let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> fun x -> 
  if n = 0 then x  
  else iter(n-1, f) (f x)
