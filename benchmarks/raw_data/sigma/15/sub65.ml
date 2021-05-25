let rec sigma (a, b, (f : int -> int)) = 
	if a = b then f a
	else f b + sigma (a, b-1, f);;
