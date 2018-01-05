let rec merge(l, m) = 
  match (l, m) with
  | ([], _) -> m
  | (_, []) -> l
  | (h1::t1, h2::t2) ->
    if h1 >= h2 then h1::merge(t1, m)
    else h2::merge(l, t2)
