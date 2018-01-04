let rec iter (n, f) k = 
	match n with
	| 0 -> k
	| 1 -> f k
	| _ -> 
		iter(n-1,f) (f k)
