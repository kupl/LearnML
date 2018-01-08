(* 2015-11380 박찬양 HW1-1 *)

let rec merge : int list * int list -> int list = fun (l1,l2) ->
  match (l1, l2) with
  | ([], _) -> l2
  | (_, []) -> l1
  | (h1::t1, h2::t2) ->
    if h1 > h2 then h1::merge(t1,l2)
    else h2::merge(l1,t2)