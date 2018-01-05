(* 2014 - 18474 kim ju hyeon  *)
(* 2017 fall PL 1_1 *)
let rec merge : int list * int list -> int list = function
  | list, []
  | [], list -> list
  | h1::t1, h2::t2 ->
    if h1 > h2 then
      h1 :: merge (t1, h2::t2)
    else
      h2 :: merge (h1::t1, t2)
