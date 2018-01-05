let rec merge (lst1, lst2) = 
	match (lst1, lst2) with
	| ([], a) -> a
	| (a, []) -> a
	| (a::b, c::d) -> if a > c then a::(merge (b, lst2))
			  else c::(merge (lst1, d))

