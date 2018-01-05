let rec sigma (a, b, fn) =
	if a > b then 0
	else (fn a) + sigma(a+1, b, fn)
