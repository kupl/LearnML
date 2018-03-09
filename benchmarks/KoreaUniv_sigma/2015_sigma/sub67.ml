let rec sigma test a b =
	if a > b then 0
	else (test a) + (sigma test (a+1) b);;
