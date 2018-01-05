let rec iter (n, f) =
	if n = 0 then 0
	else f (iter ((n-1), f))
