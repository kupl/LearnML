let rec merge ((l1:int list), (l2:int list)) : int list =
    match (l1, l2) with
    | ([], _) -> l2
    | (_, []) -> l1
    | (hd1::tl1, hd2::tl2) -> 
            (if hd1 > hd2 then hd2 :: merge(l1, tl2)
            else hd1 :: merge(tl1, l2))
