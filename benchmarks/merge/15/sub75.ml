let rec merge ((v1:int list), (v2:int list)) =
    match (v1, v2) with
    | ([], l2) -> l2
    | (l1, []) -> l1
    | (e1::t1, e2::t2) ->
            if e1 < e2 then e2::merge (v1, t2)
            else e1::merge (v2, t1);;
