(* problem 3*)


let compose f g = fun x -> f(g(x));;
let iter : int * (int -> int) -> (int -> int)
= rec fun (n,f) -> 
  if n = 0 then 1
  else compose compose fun ((fun n-1) f)