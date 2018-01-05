(* 2014 - 18474 kim ju hyeon  *)
(* 2017 fall PL 1_2 *)
let rec sigma : int * int * (int -> int) -> int = function
  (a,b,f) ->
  if a > b then 0
  else f a + sigma(a + 1, b, f)
