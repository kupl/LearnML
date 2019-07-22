(* 2014 - 18474 kim ju hyeon  *)
(* 2017 fall PL 1_3 *)
let rec iter(n, f) = 
  if n<=0 then fun x -> x
  else fun x -> iter(n-1, f) (f x)