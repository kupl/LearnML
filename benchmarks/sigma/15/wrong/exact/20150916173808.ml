let rec sigma s e f =
	match s with
	s when s <= e	-> f s
	|s		-> (f s) + (sigma (s + 1) e f)
