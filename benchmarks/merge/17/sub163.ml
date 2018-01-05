let rec merge (lst: int list * int list) : int list =
  match lst with
    | ([], r) -> r
    | (l, []) -> l
    | (l :: ls, r :: rs) ->
        if l > r then l :: merge (ls, r :: rs)
        else r :: merge (l :: ls, rs);;
