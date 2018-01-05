let merge (lst1, lst2) =
	let rec mymerge (x, l1, l2) =
		match (l1, l2) with
		| ([], l2') -> x @ l2'
		| (l1', []) -> x @ l1'
		| (h1::l1', h2::l2') ->
			if h1 > h2 then mymerge (x @ [h1], l1', h2::l2')
			else mymerge (x @ [h2], h1::l1', l2')
	in
	mymerge ([], lst1, lst2)