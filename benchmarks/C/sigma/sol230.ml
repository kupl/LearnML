(* sigma : int * int * (int -> int) -> int  *)

let rec sigma func a b =
  if a>b then 0
  else if a==b then (func a)
  else (func a) + (sigma func (a+1) b)


