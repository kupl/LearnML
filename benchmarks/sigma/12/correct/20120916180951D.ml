let rec sigma (a, b, f) =
	if a>b then invalid_arg "sigma"
	else if a=b then f a
	else f a+sigma (a+1, b, f)