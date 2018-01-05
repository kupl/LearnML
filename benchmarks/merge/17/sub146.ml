(* 2015 - 14718 Giyeon Kim HW 1 *)

(* Exercise 1 *)
let rec merge (left, right) =
    match (left, right) with
    | ([], _) -> right
    | (_, []) -> left
    | (hd1::tl1, hd2::tl2) ->
        if (hd1 > hd2) then hd1 :: (merge (tl1, right))
        else hd2 :: (merge (left, tl2))

