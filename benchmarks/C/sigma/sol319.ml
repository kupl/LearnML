let rec sigma func a b  =
  (* val sigma : int * int * (int -> int) -> int*)
  if a>b then 0 else (if a==b then (func a) else (sigma func (a+1) b)+(func a));;
