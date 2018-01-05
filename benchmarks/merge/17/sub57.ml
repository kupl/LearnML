(*
 * Dept of Physics Education
 * 2012-12666 Choi Jaehyeok
 * Homework 1, Problem 1
 *)

let rec merge: (int list * int list) -> int list = fun (a, b) ->
    match (a, b) with
    | ([], []) -> []
    | (_, []) -> a
    | ([], _) -> b
    | (hd1::tl1, hd2::tl2) ->
	if (hd1 > hd2) then hd1::(merge (tl1, b))
	else hd2::(merge (a, tl2))
