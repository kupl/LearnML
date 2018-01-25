(* problem 1*)
let rec fastexpt : int -> int -> int = fun b n ->
  if n=2 then b * b
  else if (n mod 2 = 1) then b * (fastexpt b (n-1))
  else (fastexpt b (n/2)) * (fastexpt b (n/2));;