(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> let compose f g = fun x -> f(g(x)) in
  if n = 0 then fun x -> x
  else if n = 1 then f
  else compose f (iter(n-1, f));;