(* problem 1*)
  let rec  fastexpt : int -> int -> int
  = fun b n -> let square a = a * a in if n = 0 then 1 
  else if n mod 2 = 0 then square (fastexpt b (n/2))
  else b * (fastexpt b (n-1));;