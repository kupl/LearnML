let rec merge (l1, l2) =
	match l1 with
	| [] -> l2 
	| h1::t1 -> (match l2 with
		| [] -> l1
		| h2::t2 -> if h1>h2 then h1::(merge (t1, l2)) else h2::(merge (l1, t2)))
;;