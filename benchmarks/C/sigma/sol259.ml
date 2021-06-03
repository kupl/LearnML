(* hw 1-1 *)
(* 2012-11269 DongJae Lim *)

let rec sigma f a b  =
  if a > b then 0
  else (f a) + (sigma f (a+1) b)
