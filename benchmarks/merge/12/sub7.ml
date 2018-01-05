let rec merge : int list * int list -> int list = fun (l1,l2) ->
	match (l1,l2) with
	| ([],l) | (l,[]) -> l
	| (h1::t1,h2::t2) ->
		if h1 > h2 then h1::merge(t1,l2)
		else h2::merge(l1,t2)

