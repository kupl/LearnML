let rec merge (a,b) =
  match (a,b) with 
  ([], []) -> []
  |(_,[]) -> a
  |([],_) -> b
  |((ah::at), (bh::bt)) -> (
    if (ah > bh) then ah::merge(at, b)
    else bh::merge(a, bt)
  ) 
