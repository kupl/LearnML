let rec merge (al: int list) (bl: int list) : int list =
	match al with
	| [] -> bl
	| ah::at ->
		match bl with
		| [] -> al
		| bh::bt ->
			if ah > bh then ah::(merge at bl)
			else bh::(merge al bt)