let rec merge (a, b) = 
	match a with
		| [] -> b
		| ah::at ->
			(match b with
				| [] -> a
				| bh::bt -> if ah > bh then ah::merge(at,b) else bh::merge(a,bt)
			)

		
