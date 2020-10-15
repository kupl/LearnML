let rec sigma f a b  =
	if a = b then f a
	else if a > b then 0
	else f b + sigma f a (b-1);;
