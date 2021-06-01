let iter (n, f) = 
	let rec sub_iter n =
		if n = 0 then fun x -> x
		else fun x -> f (sub_iter (n - 1) x)
	in fun x -> sub_iter n x
