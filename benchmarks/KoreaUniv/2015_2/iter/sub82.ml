let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
  if n < 0 then raise (Failure "n cannot be the neg number")
  else
    (match n with
      0 -> fun x->x 
     |1 -> f
     |_ -> iter(n-1, fun x -> f (f x) ) ) 
