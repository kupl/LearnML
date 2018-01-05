let rec merge (a, b) =
	match (a, b) with
	| ([], _) -> b
	| (_, []) -> a
	| (h1::t1, h2::t2) ->
		if h1>h2 then h1::(merge (t1, b))
		else h2::(merge (a, t2))
