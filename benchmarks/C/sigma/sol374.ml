(* 2014 - 18474 kim ju hyeon  *)
(* 2017 fall PL 1_2 *)
let rec sigma f a b =
  if a > b then 0
  else f a + sigma f (a+1) b
