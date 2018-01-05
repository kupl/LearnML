let rec merge : int list * int list -> int list = fun (l1, l2) ->
	match l1 with 
	| hd1::tl1 -> (match l2 with
				| hd2::tl2 -> if hd1 > hd2 then hd1::(merge (tl1, l2))
							  else if hd1 = hd2 then hd1::(merge (tl1, tl2))
							  else hd2::(merge (l1, tl2))
				| [] -> l1)
	| [] -> l2


