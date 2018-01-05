let rec iter (n, f) i =
	match n with
	| 0 -> i
	| _ -> (iter (n - 1, f) (f i))

