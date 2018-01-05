

let rec merge (p : int list * int list) : int list = 
	match p with 
	| (x, []) -> x
	| ([], x) -> x
	| (h1::t1, h2::t2) -> (
		if h1 >= h2 then ( h1 :: ( merge (t1, h2::t2) ) )
		else ( h2 :: (merge (h1::t1, t2) ) )
	)

