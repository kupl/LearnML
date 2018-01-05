(*
  컴퓨터공학부/2015-21233/김종권
  Homework 1-1
*)
let rec merge (l1, l2) =
  match l1, l2 with
  | h1 :: t1, h2 :: t2 ->
    if h1 > h2 then
      h1 :: merge (t1, h2 :: t2)
    else if h1 < h2 then
      h2 :: merge (h1 :: t1, t2)
    else
      h1 :: h2 :: merge (t1, t2) 
  | hd :: tl, _ ->
    hd :: merge (tl,[]) 
  | _, hd :: tl ->
    hd :: merge ([],tl)
  | _, _ ->
    []
