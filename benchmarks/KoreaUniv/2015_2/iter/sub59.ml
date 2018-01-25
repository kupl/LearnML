let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
  let compose ((f, g) : (int -> int) * (int -> int)) (x : int) : int = f (g x) in
  if n = 0 then f
  else if n > 0 then compose(f, iter(n-1, f) )
  else f;;

