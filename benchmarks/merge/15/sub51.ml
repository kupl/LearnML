let rec merge : int list * int list -> int list = fun (a, b) ->
	match a with
	| [] -> 
		(match b with
		| [] -> []
		| _ -> b
		)
	| aa::at ->
		(match b with
		| [] -> a
		| bb::bt -> if aa > bb then aa::(merge (at, b)) else bb::(merge (a, bt))
		)	