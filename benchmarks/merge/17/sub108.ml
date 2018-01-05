let rec merge((l1 : int list), (l2 : int list)) : int list = 
	match (l1, l2) with
	| ([], _) -> l2
	| (_, []) -> l1
	| (a::l3, b::l4) ->
	(if a>b then a::(merge (l3, l2))
	else b::(merge (l1, l4)) )
;;
