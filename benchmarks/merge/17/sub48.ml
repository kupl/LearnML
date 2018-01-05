let rec merge ((lst1 : int list), (lst2 : int list)) : int list =
    match (lst1, lst2) with
    | ([], _) -> lst2
    | (_, []) -> lst1
    | (hd1::tl1, hd2::tl2) ->
            if hd1 > hd2 then hd1::(merge (tl1, lst2))
            else hd2::(merge (lst1, tl2))
