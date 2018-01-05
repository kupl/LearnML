let rec merge : (int list*int list) -> int list = fun (l1, l2) ->
	match (l1,l2) with
	| ([],[]) -> []
	| ([],l2) -> l2
	| (l1,[]) -> l1
	| (h1::t1, h2::t2) ->
		if (h1 > h2) then h1::merge (t1,l2)
		else if (h1 = h2) then h1::h2::merge (t1,t2)
		else h2::merge (l1,t2)
