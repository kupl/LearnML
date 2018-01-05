let rec merge l =
	match l with
	[], l1 -> l1
	| l1, [] -> l1
	| l1, l2 ->
		let h1 = List.hd l1 in
		let h2 = List.hd l2 in
		if h1 > h2 then h1::(merge (List.tl l1, l2)) else
			if h2 > h1 then h2::(merge (l1, List.tl l2)) else
				h1::(merge (List.tl l1, l2))
