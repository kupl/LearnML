let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f(* let cal n f x = f x 
  if n>1 then iter (n-1) f + x
                else f )*)

