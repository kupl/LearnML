let rec iter (n,f) = 
	let identity s = s in
	let newiter x = (iter((n-1),f)) (f x) in
	if n < 0 then invalid_arg "iter"
	else if n=0 then identity
	else newiter

