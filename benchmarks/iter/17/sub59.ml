let rec iter (n, f) =
	if n > 0 then fun x -> f(iter (n-1, f) x)
	else fun x -> x
