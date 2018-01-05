(*
 * CSE / 2013-11426 / Im Dongyeop
 * Homework 1 : Exercise 1
 *)

let rec merge ((list1: int list), (list2: int list)): int list =
	match (list1, list2) with
	| ([], _) -> list2
	| (_, []) -> list1
	| (hd1::tl1, hd2::tl2) ->
		if (hd1 > hd2)
			then (hd1::merge(tl1, list2))
		else
			(hd2::merge(list1, tl2))
