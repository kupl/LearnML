(* 컴퓨터공학과/2017-34165/김성국/1-1 *)
let rec merge ((x : int list), (y : int list)) : int list =
(* let rec merge (x, y) = *)
  match (x, y) with
  | ([], y) -> y
  | (x, []) -> x
  | (xh::xt, yh::yt) ->
    if xh > yh
    then xh::(merge (xt, y))
    else yh::(merge (x, yt))
