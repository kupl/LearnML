let rec iter (n,f) a =
	match n with
		| 0 -> a
		| _ -> (iter ((n-1),f)) (f a)