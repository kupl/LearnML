exception Large_a

let rec sigma f a b =
	if a > b then raise Large_a
	else if a = b then (f a)
	else (f a) + sigma f (a+1) b
