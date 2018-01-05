let rec merge ((l: int list), (r: int list)) : (int list) =
    match (l, r) with
    | ([], _) -> r
    | (_, []) -> l
    | (lh :: lt, rh :: rt) ->
            if lh > rh then lh :: merge(lt, r)
            else rh :: merge(l, rt)
