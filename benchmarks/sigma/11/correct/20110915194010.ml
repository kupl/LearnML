let rec sigma (a,b,f) =
	match (a - b) with
		| 0 -> (f a)
		| _ -> (f a) + (sigma ((a + 1),b,f))