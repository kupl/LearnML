let rec iter (n, f) s =
	if (n = 0) then s
	else iter ((n-1), f) (f s)
