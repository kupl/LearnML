let rec sigma f a b =
	if b < a then 0
	else if a == b then (f a)
	else (f a) + sigma f (a+1) b
