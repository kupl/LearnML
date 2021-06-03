let rec sigma fn a b =
	if a > b then 0
	else (fn a) + sigma fn (a+1) b
