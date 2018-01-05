let rec merge (a, b) =
  match (a, b) with
  | ([], b) -> b
  | (a, []) -> a
  | (ha::ta, hb::tb) when (ha>hb) -> ha::(merge (ta, b))
  | _ -> (List.hd b)::(merge (a, (List.tl b)))
