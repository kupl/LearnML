let merge ((list1 : int list), (list2 : int list)) = 
	let rec compare l1 l2 res = 
		match l1 with
		| [] -> res @ l2
		| h1::t1 -> match l2 with
			| [] -> res @ l1
			| h2::t2 -> if h1 > h2 then compare t1 l2 (res @ [h1])
						else compare l1 t2 (res @ [h2]) in
	compare list1 list2 [];;
