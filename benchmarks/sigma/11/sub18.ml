let rec sigma(a, b, f) = 
	if a-1 = b then 0
	else sigma(a+1, b, f) + f(a)