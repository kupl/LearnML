(*
    Homework 1, Exercise 1
    2015-15894 Jonghoon Won
    Sep 14, 2017
*)

let rec merge : int list * int list -> int list = fun (lst1, lst2) ->
    match (lst1, lst2) with
    | (_, []) -> lst1
    | ([], _) -> lst2
    | (hd1::tl1, hd2::tl2) -> (
        if hd1 > hd2 then hd1::merge (tl1, lst2)
        else hd2::merge (lst1, tl2)
        )
