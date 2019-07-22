let rec filter pred lst = 
	let rec check l r = 
		match l with
		[] -> r
		|h::t -> if pred h then (check t r)@[h] else check t r
	in check lst []
