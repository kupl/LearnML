let rec merge : int list * int list -> int list = fun (ilist1, ilist2) ->
    match (ilist1, ilist2) with
    | ([], lst)
    | (lst, []) -> lst
    | (hd1::tl1, hd2::tl2) ->
            if hd1 > hd2 then hd1::merge (tl1, hd2::tl2)
            else hd2::merge (hd1::tl1, tl2)
