let rec iter (n, ft) a =
	if n=0 then a
	else ft (iter (n-1, ft) a)