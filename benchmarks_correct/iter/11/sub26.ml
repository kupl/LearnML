let rec iter (n, f) x =
	if n=0 then x
	else f (iter (n-1, f) x)
