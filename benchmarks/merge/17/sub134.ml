let rec merge (lp : (int list * int list)) : int list =
  match lp with
  | ([],l2) -> l2
  | (l1,[]) -> l1
  | (h1::t1,h2::t2) ->
      if h1 > h2 then
        h1::merge(t1,h2::t2)
      else
        h2::merge(h1::t1,t2)
