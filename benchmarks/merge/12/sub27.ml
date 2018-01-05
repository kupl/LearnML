(* hw1-1 *)
(* 2010-11687 Keunjun Choi *)

let rec merge (a, b) : int list  =
	match (a, b) with
	| ([], _) -> b
	| (_, []) -> a
	| (a1::a2, b1::b2) ->
		(if a1>b1 then 
			a1::merge (a2, b)
		else
			b1::merge (a, b2))
