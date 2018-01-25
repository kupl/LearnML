
(* 4 *)
let rec product f a b : int
= if a = b then f a
  else a*(product f (a+1) b);;
