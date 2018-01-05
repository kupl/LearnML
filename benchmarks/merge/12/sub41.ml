let rec merge (l1, l2) =
        match (l1, l2) with
        | ([], l2) -> l2
        | (l1, []) -> l1
        | (hd1::tl1, hd2::tl2) ->
                if (hd1 > hd2) then hd1::(merge (tl1, l2))
                else if (hd1 < hd2) then hd2::(merge (l1, tl2))
                else hd1::(merge (tl1, l2))
