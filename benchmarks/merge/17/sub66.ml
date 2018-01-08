let rec merge : (int list * int list) -> int list = fun (list1, list2) ->
  match (list1, list2) with 
  | ([], _) -> list2
  | (_, []) -> list1
  | (h1::t1, h2::t2) -> 
    if (h1 > h2) then (h1 :: merge (t1, list2))
    else (h2 :: merge (t2, list1))