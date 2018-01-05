let rec merge (lists : int list * int list) = match lists with
    | list1, [] -> list1
    | [], list2 -> list2
    | (hd1::tl1), (hd2::tl2) ->
        if hd1 > hd2 then hd1 :: merge (tl1, hd2 :: tl2)
        else hd2 :: merge (hd1 :: tl1, tl2)
