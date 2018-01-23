let rec merge (a, b) =
  match (a, b) with
  | ([], b) -> b
  | (a, []) -> a
  | (ha::ta, hb::tb) -> 
    if (ha>hb) then ha::(merge (ta, b))
    else (List.hd b)::(merge (a, (List.tl b)))
