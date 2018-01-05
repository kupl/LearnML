let rec sigma (a, b, f) =
	if b < a then 0
	else if a == b then (f a)
	else (f a) + sigma(a+1, b, f)
