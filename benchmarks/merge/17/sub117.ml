(* 2014-19180 You JooSeung Question 1*)

let rec merge((l1: int list),(l2: int list)): int list = match (l1, l2) with
	|([], _) -> l2
	|(_, []) -> l1
	| (h1::t1, h2::t2) ->
		if h1 < h2 then h2 :: merge (l1,t2)
		else h1:: merge(t1,l2)
