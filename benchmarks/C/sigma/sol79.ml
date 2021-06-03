let rec sigma f a b =
	match (a - b) with
		| 0 -> (f a)
		| _ -> (f a) + (sigma f (a+1) b)