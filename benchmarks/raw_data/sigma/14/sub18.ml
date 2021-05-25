(* sigma : int * int * (int -> int) -> int  *)

let rec sigma (a, b, func) = 
  if a>b then 0
  else if a==b then (func a)
  else (func a) + (sigma (a+1, b, func))


