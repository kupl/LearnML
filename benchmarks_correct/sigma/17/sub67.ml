let rec sigma f n m =
	if (n<=m) 
		then ((f n) + (sigma f (n+1) m))
		else 0
