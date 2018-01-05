let rec merge (l1, l2) : int list =
	match l1 with
	| h1::t1 ->  
		(match l2 with
		| h2::t2 -> 
			(if h1 < h2 then h2::merge(l1, t2)
			else h1::merge(t1, l2))
		| _ -> l1)
  | _ -> l2