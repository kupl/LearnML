let rec merge ((a: int list), (b: int list)) : int list  = 
	match (a, b) with
	| ([], _) -> b
	| (_, []) -> a
	| (ah::at, bh::bt) -> if ah > bh then ah::merge(at, b) else bh::merge(a, bt)
