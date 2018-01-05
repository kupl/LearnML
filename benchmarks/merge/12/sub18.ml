(* ex1 *)
let rec merge (lst1, lst2) = 
	match (lst1, lst2) with
	  ([], []) -> []
	| (h::t, []) -> h::t
	| ([], h::t) -> h::t
	| (h1::t1, h2::t2) -> (if h1 >= h2 then h1 :: h2 :: merge (t1, t2)
			       else h2 :: h1 :: merge (t1, t2))